-- | The CEK machine.
-- The CEK machine relies on variables having non-equal 'Unique's whenever they have non-equal
-- string names. I.e. 'Unique's are used instead of string names. This is for efficiency reasons.
-- The CEK machines handles name capture by design.

{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ImplicitParams           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NPlusKPatterns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module UntypedPlutusCore.Evaluation.Machine.Cek.Internal
    -- See Note [Compilation peculiarities].
    ( EvaluationResult(..)
    , CekValue(..)
    , CekUserError(..)
    , CekEvaluationException
    , CekBudgetSpender(..)
    , ExBudgetInfo(..)
    , ExBudgetMode(..)
    , CekCarryingM (..)
    , CekM
    , ErrorWithCause(..)
    , EvaluationError(..)
    , ExBudgetCategory(..)
    , StepKind(..)
    , PrettyUni
    , extractEvaluationResult
    , runCek
    )
where

import           ErrorCode
import           PlutusPrelude

import           UntypedPlutusCore.Core
import           UntypedPlutusCore.Subst

import           PlutusCore.Constant
import           PlutusCore.Evaluation.Machine.ExBudget
import           PlutusCore.Evaluation.Machine.ExMemory
import           PlutusCore.Evaluation.Machine.Exception
import           PlutusCore.Evaluation.Machine.MachineParameters
import           PlutusCore.Evaluation.Result
import           PlutusCore.Name
import           PlutusCore.Pretty

import           Data.Text.Prettyprint.Doc

import           UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts (CekMachineCosts (..))

import           Control.Lens.Review
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Array
import           Data.DList                                               (DList)
import qualified Data.DList                                               as DList
import           Data.Hashable                                            (Hashable)
import qualified Data.Kind                                                as GHC
import           Data.Proxy
import           Data.STRef
import           Data.Semigroup                                           (stimes)
import           Data.Word64Array.Word8
import           Universe

{- Note [Compilation peculiarities]
READ THIS BEFORE TOUCHING ANYTHING IN THIS FILE

Don't use @StrictData@, it makes the machine slower by several percent.

Exporting the 'computeCek' function from this module causes the CEK machine to become slower by
up to 25%. I repeat: just adding 'computeCek' to the export list makes the evaluator substantially
slower. The reason for this is that with 'computeCek' exported the generated GHC Core is much worse:
it contains more lambdas, allocates more stuff etc. While perhaps surprising, this is not an
unusual behavior of the compiler as https://wiki.haskell.org/Performance/GHC explains:

> Indeed, generally speaking GHC will inline across modules just as much as it does within modules,
> with a single large exception. If GHC sees that a function 'f' is called just once, it inlines it
> regardless of how big 'f' is. But once 'f' is exported, GHC can never see that it's called exactly
> once, even if that later turns out to be the case. This inline-once optimisation is pretty
> important in practice.
>
> So: if you care about performance, do not export functions that are not used outside the module
> (i.e. use an explicit export list, and keep it as small as possible).

Now clearly 'computeCek' cannot be inlined in 'runCek' whether it's exported or not, since
'computeCek' is recursive. However:

1. GHC is _usually_ smart enough to perform the worker/wrapper transformation and inline the wrapper
   (however experiments have shown that sticking the internals of the CEK machine, budgeting modes
   and the API into the same file prevents GHC from performing the worker/wrapper transformation for
   some reason likely related to "we've been compiling this for too long, let's leave it at that"
2. GHC seems to be able to massage the definition of 'computeCek' into something more performant
   making use of knowing exactly how 'computeCek' is used, essentially tailoring the definition of
   'computeCek' for a particular invocation in 'runCek'

Hence we don't export 'computeCek' and instead define 'runCek' in this file and export it, even
though the rest of the user-facing API (which 'runCek' is a part of) is defined downstream.

Another problem is handling mutual recursion in the 'computeCek'/'returnCek'/'forceEvaluate'/etc
family. If we keep these functions at the top level, GHC won't be able to pull the constraints out of
the family (confirmed by inspecting Core: GHC thinks that since the superclass constraints
populating the dictionary representing the @Ix fun@ constraint are redundant, they can be replaced
with calls to 'error' in a recursive call, but that changes the dictionary and so it can no longer
be pulled out of recursion). But that entails passing a redundant argument around, which slows down
the machine a tiny little bit.

Hence we define a number of the functions as local functions making use of a
shared context from their parent function. This also allows GHC to inline almost
all of the machine into a single definition (with a bunch of recursive join
points in it).

In general, it's advised to run benchmarks (and look at Core output if the results are suspicious)
on any changes in this file.

Finally, it's important to put bang patterns on any Int arguments to ensure that GHC unboxes them:
this can make a surprisingly large difference.
-}

{- Note [Scoping]
The CEK machine does not rely on the global uniqueness condition, so the renamer pass is not a
prerequisite. The CEK machine correctly handles name shadowing.
-}

data StepKind
    = BConst
    | BVar
    | BLamAbs
    | BApply
    | BDelay
    | BForce
    | BBuiltin -- Cost of evaluating a Builtin AST node, not the function itself
    deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
    deriving anyclass (NFData, Hashable)

cekStepCost :: CekMachineCosts -> StepKind -> ExBudget
cekStepCost costs = \case
    BConst   -> cekConstCost costs
    BVar     -> cekVarCost costs
    BLamAbs  -> cekLamCost costs
    BApply   -> cekApplyCost costs
    BDelay   -> cekDelayCost costs
    BForce   -> cekForceCost costs
    BBuiltin -> cekBuiltinCost costs

data ExBudgetCategory fun
    = BStep StepKind
    | BBuiltinApp fun  -- Cost of evaluating a fully applied builtin function
    | BStartup
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)
instance Show fun => Pretty (ExBudgetCategory fun) where
    pretty = viaShow

instance ExBudgetBuiltin fun (ExBudgetCategory fun) where
    exBudgetBuiltin = BBuiltinApp

{- Note [Show instance for BuiltinRuntime]
We need to be able to print 'CekValue's and for that we need a 'Show' instance for 'BuiltinRuntime',
but functions are not printable and hence we provide a dummy instance.
-}

-- See Note [Show instance for BuiltinRuntime].
instance Show (BuiltinRuntime (CekValue uni fun)) where
    show _ = "<builtin_runtime>"

-- 'Values' for the modified CEK machine.
data CekValue uni fun =
    -- This bang gave us a 1-2% speed-up at the time of writing.
    VCon !(Some (ValueOf uni))
  | VDelay (ETerm uni fun) !(CekValEnv uni fun)
  | VLamAbs !Unique (ETerm uni fun) !(CekValEnv uni fun)
  | VBuiltin            -- A partial builtin application, accumulating arguments for eventual full application.
      !fun                   -- So that we know, for what builtin we're calculating the cost.
                             -- TODO: any chance we could sneak this into 'BuiltinRuntime'
                             -- where we have a partially instantiated costing function anyway?
      (ETerm uni fun) -- This must be lazy. It represents the partial application of the
                             -- builtin function that we're going to run when it's fully saturated.
                             -- We need the 'Term' to be able to return it in case full saturation
                             -- is never achieved and a partial application needs to be returned
                             -- in the result. The laziness is important, because the arguments are
                             -- discharged values and discharging is expensive, so we don't want to
                             -- do it unless we really have to. Making this field strict resulted
                             -- in a 3-4.5% slowdown at the time of writing.
      (CekValEnv uni fun)    -- For discharging.
      !(BuiltinRuntime (CekValue uni fun))  -- The partial application and its costing function.
                                            -- Check the docs of 'BuiltinRuntime' for details.
    deriving (Show)

type CekValEnv uni fun = UniqueMap TermUnique (CekValue uni fun)

-- | The CEK machine is parameterized over a @spendBudget@ function that has (roughly) the same type
-- as the one from the 'SpendBudget' class (and so the @SpendBudget@ instance for 'CekM'
-- defers to the function stored in the environment). This makes the budgeting machinery extensible
-- and allows us to separate budgeting logic from evaluation logic and avoid branching on the union
-- of all possible budgeting state types during evaluation.
newtype CekBudgetSpender uni fun s = CekBudgetSpender
    { unCekBudgetSpender :: ExBudgetCategory fun -> ExBudget -> CekM uni fun s ()
    }

-- General enough to be able to handle a spender having one, two or any number of 'STRef's
-- under the hood.
-- | Runtime budgeting info.
data ExBudgetInfo cost uni fun s = ExBudgetInfo
    { _exBudgetModeSpender  :: !(CekBudgetSpender uni fun s)  -- ^ A spending function.
    , _exBudgetModeGetFinal :: !(ST s cost)                   -- ^ For accessing the final state.
    }

-- We make a separate data type here just to save the caller of the CEK machine from those pesky
-- 'ST'-related details.
-- | A budgeting mode to execute the CEK machine in.
newtype ExBudgetMode cost uni fun = ExBudgetMode
    { unExBudgetMode :: forall s. ST s (ExBudgetInfo cost uni fun s)
    }

{- Note [Cost slippage]
Tracking the budget usage for every step in the machine adds a lot of overhead. To reduce this,
we adopt a technique which allows some overshoot of the budget ("slippage"), but only a bounded
amount.

To do this we:
- Track all the machine steps of all kinds in an optimized way in a 'WordArray'.
- Actually "spend" the budget when we've done more than some fixed number of steps, or at the end.

This saves a *lot* of time, at the cost of potentially overshooting the budget by slippage*step_cost,
which is okay so long as we bound the slippage appropriately.

Note that we're only proposing to do this for machine steps, since it's plausible that we can track
them in an optimized way. Builtins are more complicated (and infrequent), so we can just budget them
properly when we hit them.

There are two options for how to bound the slippage:
1. As a fixed number of steps
2. As a proportion of the overall budget

Option 2 initially seems much better as a bound: if we run N scripts with an overall budget of B, then
the potential overrun from 1 is N*slippage, whereas the overrun from 2 is B*slippage. That is, 2
says we always overrun by a fraction of the total amount of time you were expecting, whereas 1 says
it depends how many scripts you run... so if I send you a lot of small scripts, I could cause a lot
of overrun.

However, it turns out (empirically) that we can pick a number for 1 that gives us most of the speedup, but such
that the maximum overrun is negligible (e.g. much smaller than the "startup cost"). So in the end
we opted for option 1, which also happens to be simpler to implement.
-}

{- Note [Structure of the step counter]
The step counter is kept in a 'WordArray', which is 8 'Word8's packed into a single 'Word64'.
This happens to suit our purposes exactly, as we want to keep a counter for each kind of step
that we know about (of which there are 7) and one for the total number.

We keep the counters for each step in the first 7 indices, so we can index them simply by using
the 'Enum' instance of 'StepKind', and the total counter in the last index.

Why use a 'WordArray'? It optimizes well, since GHC can often do quite a lot of constant-folding
on the bitwise operations that get emitted. We are restricted to counters of size 'Word8', but this
is fine since we will reset when we get to 200 steps.

The sharp-eyed reader might notice that the benchmarks in 'word-array' show that 'PrimArray'
seems to be faster! However, we tried that and it was slower overall, we don't know why.
-}

type Slippage = Word8
-- See Note [Cost slippage]
-- | The default number of slippage (in machine steps) to allow.
defaultSlippage :: Slippage
defaultSlippage = 200

{- Note [Implicit parameters in the machine]
The traditional way to pass context into a function is to use 'ReaderT'. However, 'ReaderT' has some
disadvantages.
- It requires threading through the context even where you don't need it (every monadic bind)
- It *can* often be optimized away, but this requires GHC to be somewhat clever and do a lot of
  case-of-case to lift all the arguments out.

Moreover, if your context is global (i.e. constant across the lifetime of the monad, i.e. you don't
need 'local'), then you're buying some extra power (the ability to pass in a different context somewhere
deep inside the computation) which you don't need.

There are three main alternatives:
- Explicit function parameters. Simple, doesn't get tied up in the Monad operations, *does* still
present the appearance of letting you do 'local'. But a bit cluttered.
- Implicit parameters. A bit esoteric, can be bundled up into a constraint synonym and just piped to
where they're needed, essentially the same as explicit parameters in terms of runtime.
- Constraints via 'reflection'. Quite esoteric, *does* get you global parameters (within their scope),
bit of a hassle threading around all the extra type parameters.

We're using implicit parameters for now, which seems to strike a good balance of speed and convenience.
I haven't tried 'reflection' in detail, but I believe the main thing it would do is to make the parameters
global - but we already have this for most of the hot functions by making them all local definitions, so
they don't actually take the context as an argument even at the source level.
-}

-- | Implicit parameter for the builtin runtime.
type GivenCekRuntime uni fun = (?cekRuntime :: (BuiltinsRuntime fun (CekValue uni fun)))
-- | Implicit parameter for the log emitter reference.
type GivenCekEmitter s = (?cekEmitter :: (Maybe (STRef s (DList String))))
-- | Implicit parameter for budget spender.
type GivenCekSpender uni fun s = (?cekBudgetSpender :: (CekBudgetSpender uni fun s))
type GivenCekSlippage = (?cekSlippage :: Slippage)
type GivenCekCosts = (?cekCosts :: CekMachineCosts)

-- | Constraint requiring all of the machine's implicit parameters.
type GivenCekReqs uni fun s = (GivenCekRuntime uni fun, GivenCekEmitter s, GivenCekSpender uni fun s, GivenCekSlippage, GivenCekCosts)

data CekUserError
    = CekOutOfExError ExRestrictingBudget -- ^ The final overspent (i.e. negative) budget.
    | CekEvaluationFailure -- ^ Error has been called or a builtin application has failed
    deriving (Show, Eq, Generic, NFData)

instance HasErrorCode CekUserError where
    errorCode CekEvaluationFailure {} = ErrorCode 37
    errorCode CekOutOfExError {}      = ErrorCode 36

{- Note [Being generic over 'term' in errors]
Our error for the CEK machine carries a term with it as a possible 'cause'. This is defined in
terms of an underlying type that is generic over this 'term' type.

The point is that in many different cases we can annotate an evaluation failure with a 'Term' that
caused it. Originally we were using 'CekValue' instead of 'Term', however that meant that we had to
ignore some important cases where we just can't produce a 'CekValue', for example if we encounter
a free variable, we can't turn it into a 'CekValue' and report the result as the cause of the
failure, which is bad. 'Term' is strictly more general than 'CekValue' and so we can always
1. report things like free variables
2. report a 'CekValue' turned into a 'Term' via 'dischargeCekValue'
We need the latter, because the constant application machinery, in the context of the CEK machine,
expects a list of 'CekValue's and so in the event of failure it has to report one of those
arguments, so we have no option but to call the constant application machinery with 'CekValue'
being the cause of a potential failure. But as mentioned, turning a 'CekValue' into a 'Term' is
no problem and we need that elsewhere anyway, so we don't need any extra machinery for calling the
constant application machinery over a list of 'CekValue's and turning the cause of a possible
failure into a 'Term', apart from the straightforward generalization of the error type.

Error handling is arranged as follows: we allow the CEK machine to throw terms and the builtin
appliation machinery to throw values and in 'runCekM' we catch both kinds of errors and turn
the latter into the former, so that to the outside it looks like the CEK machine is pure and
can only return errors with terms in them, not values.
-}

{- Note [Being generic over @term@ in 'CekM']
We have a @term@-generic version of 'CekM' called 'CekCarryingM', which itself requires a
@term@-generic version of 'CekEvaluationException' called 'CekEvaluationExceptionCarrying'.
This enables us to implement a 'MonadError' instance that allow for throwing errors in both the CEK
machine and the builtin application machinery (as Note [Being generic over 'term' in errors]
explains, those are different kinds of errors) and otherwise we'd have to have more plumbing between
these two and turning the builtin application machinery's 'MonadError' constraint into an explicit
'Either' just to dispatch on it in the CEK machine and rethrow the error non-purely  (we used to
have that) has to cost some. Plus, being @term@-generic also enables us to throw actual exceptions
using the normal 'throwError', 'throwing_', 'throwingWithCause' etc business instead of duplicating
all that API for exceptions (we used to have @lookupBuiltinExc@, @throwingWithCauseExc@ etc).
-}

-- See Note [Being generic over @term@ in 'CekM'].
type CekCarryingM :: GHC.Type -> (GHC.Type -> GHC.Type) -> GHC.Type -> GHC.Type -> GHC.Type -> GHC.Type
-- | The monad the CEK machine runs in.
newtype CekCarryingM term uni fun s a = CekCarryingM
    { unCekCarryingM :: ST s a
    } deriving newtype (Functor, Applicative, Monad)

type CekM uni fun = CekCarryingM (Term Name uni fun ()) uni fun

-- | The CEK machine-specific 'EvaluationException'.
type CekEvaluationExceptionCarrying term fun =
    EvaluationException CekUserError (MachineError fun) term

type CekEvaluationException uni fun = CekEvaluationExceptionCarrying (Term Name uni fun ()) fun

-- | The set of constraints we need to be able to print things in universes, which we need in order to throw exceptions.
type PrettyUni uni fun = (GShow uni, Closed uni, Pretty fun, Typeable uni, Typeable fun, Everywhere uni PrettyConst)

{- Note [Throwing exceptions in ST]
This note represents MPJ's best understanding right now, might be wrong.

We use a moderately evil trick to throw exceptions in ST, but unlike the evil trick for catching them, it's hidden.

The evil is that the 'MonadThrow' instance for 'ST' uses 'unsafeIOToST . throwIO'! Sneaky! The author has marked it
"Trustworthy", no less. However, I believe this to be safe for basically the same reasons as our trick to catch
exceptions is safe, see Note [Catching exceptions in ST]
-}

{- Note [Catching exceptions in ST]
This note represents MPJ's best understanding right now, might be wrong.

We use a moderately evil trick to catch exceptions in ST. This uses the unsafe ST <-> IO conversion functions to go into IO,
catch the exception, and then go back into ST.

Why is this okay? Recall that IO ~= ST RealWorld, i.e. it is just ST with a special thread token. The unsafe conversion functions
just coerce from one to the other. So the thread token remains the same, it's just that we'll potentially leak it from ST, and we don't
get ordering guarantees with other IO actions.

But in our case this is okay, because:

1. We do not leak the original ST thread token, since we only pass it into IO and then immediately back again.
2. We don't have ordering guarantees with other IO actions, but we don't care because we don't do any side effects, we only catch a single kind of exception.
3. We *do* have ordering guarantees between the throws inside the ST action and the catch, since they are ultimately using the same thread token.
-}

-- | Call 'dischargeCekValue' over the received 'CekVal' and feed the resulting 'Term' to
-- 'throwingWithCause' as the cause of the failure.
throwingDischarged
    :: (PrettyUni uni fun)
    => AReview (EvaluationError CekUserError (MachineError fun)) t
    -> t
    -> CekValue uni fun
    -> CekM uni fun s x
throwingDischarged l t = throwingWithCause l t . Just . etermToTerm . dischargeCekValue

-- | Enable throwing/catching 'CekValue's within the received action and
-- catching 'Term's outside of it.
withCekValueErrors
    :: CekCarryingM (CekValue uni fun) uni fun s a
    -> CekCarryingM (Term Name uni fun ()) uni fun s a
withCekValueErrors = coerce

-- | Enable throwing/catching 'Term's within the received action and
-- catching 'CekValue's outside of it.
withTermErrors
    :: CekCarryingM (Term Name uni fun ()) uni fun s a
    -> CekCarryingM (CekValue uni fun) uni fun s a
withTermErrors = coerce

instance (PrettyUni uni fun, PrettyPlc term, Typeable term) =>
            MonadError (CekEvaluationExceptionCarrying term fun) (CekCarryingM term uni fun s) where
    -- See Note [Throwing exceptions in ST].
    throwError = CekCarryingM . throwM

    -- See Note [Catching exceptions in ST].
    a `catchError` h = CekCarryingM . unsafeIOToST $ aIO `catch` hIO where
        aIO = unsafeRunCekCarryingM a
        hIO = unsafeRunCekCarryingM . h

        -- | Unsafely run a 'CekCarryingM' computation in the 'IO' monad by converting the
        -- underlying 'ST' to it.
        unsafeRunCekCarryingM :: CekCarryingM term uni fun s a -> IO a
        unsafeRunCekCarryingM = unsafeSTToIO . unCekCarryingM

-- It would be really nice to define this instance, so that we can use 'makeKnown' directly in
-- the 'CekM' monad without the 'WithEmitterT' nonsense. Unfortunately, GHC doesn't like
-- implicit params in instance contexts. As GHC's docs explain:
--
-- > Reason: exactly which implicit parameter you pick up depends on exactly where you invoke a
-- > function. But the "invocation" of instance declarations is done behind the scenes by the
-- > compiler, so it's hard to figure out exactly where it is done. The easiest thing is to outlaw
-- > the offending types.
-- instance GivenCekEmitter s => MonadEmitter (CekM uni fun s) where
--     emit = emitCek

instance AsEvaluationFailure CekUserError where
    _EvaluationFailure = _EvaluationFailureVia CekEvaluationFailure

instance Pretty CekUserError where
    pretty (CekOutOfExError (ExRestrictingBudget res)) =
        group $ "The budget was overspent. Final negative state:" <+> pretty res
    pretty CekEvaluationFailure = "The provided Plutus code called 'error'."

spendBudgetCek :: GivenCekSpender uni fun s => ExBudgetCategory fun -> ExBudget -> CekM uni fun s ()
spendBudgetCek = let (CekBudgetSpender spend) = ?cekBudgetSpender in spend

emitCek :: GivenCekEmitter s => String -> CekM uni fun s ()
emitCek str =
    let mayLogsRef = ?cekEmitter
    in case mayLogsRef of
        Nothing      -> pure ()
        Just logsRef -> CekCarryingM $ modifySTRef logsRef (`DList.snoc` str)

-- see Note [Scoping].
-- | Instantiate all the free variables of a term by looking them up in an environment.
-- Mutually recursive with dischargeCekVal.
dischargeCekValEnv :: CekValEnv uni fun -> ETerm uni fun -> ETerm uni fun
dischargeCekValEnv !valEnv =
    -- We recursively discharge the environments of Cek values, but we will gradually end up doing
    -- this to terms which have no free variables remaining, at which point we won't call this
    -- substitution function any more and so we will terminate.
    etermSubstFreeNames $ \name -> do
        val <- lookupUnique (coerce name) valEnv
        Just $ dischargeCekValue val
--
-- | Convert a 'CekValue' into a 'Term' by replacing all bound variables with the terms
-- they're bound to (which themselves have to be obtain by recursively discharging values).
dischargeCekValue :: CekValue uni fun -> ETerm uni fun
dischargeCekValue = \case
    VCon     val           -> EConstant val
    VDelay   body env      -> dischargeCekValEnv env $ EDelay body
    -- 'computeCek' turns @LamAbs _ name body@ into @VLamAbs name body env@ where @env@ is an
    -- argument of 'computeCek' and hence we need to start discharging outside of the reassembled
    -- lambda, otherwise @name@ could clash with the names that we have in @env@.
    VLamAbs  name body env -> dischargeCekValEnv env $ ELamAbs name body
    -- We only discharge a value when (a) it's being returned by the machine,
    -- or (b) it's needed for an error message.
    VBuiltin _ term env _  -> dischargeCekValEnv env term

instance (Closed uni, GShow uni, uni `Everywhere` PrettyConst, Pretty fun) =>
            PrettyBy PrettyConfigPlc (CekValue uni fun) where
    prettyBy cfg = prettyBy cfg . dischargeCekValue

type instance UniOf (CekValue uni fun) = uni

instance FromConstant (CekValue uni fun) where
    fromConstant = VCon

instance AsConstant (CekValue uni fun) where
    asConstant (VCon val) = pure val
    asConstant term       = throwNotAConstant term

{-|
The context in which the machine operates.

Morally, this is a stack of frames, but we use the "intrusive list" representation so that
we can match on context and the top frame in a single, strict pattern match.
-}
data Context uni fun
    = FrameApplyFun !(CekValue uni fun) !(Context uni fun)                         -- ^ @[V _]@
    | FrameApplyArg !(CekValEnv uni fun) (ETerm uni fun) !(Context uni fun) -- ^ @[_ N]@
    | FrameForce !(Context uni fun)                                               -- ^ @(force _)@
    | NoFrame
    deriving (Show)

toExMemory :: (Closed uni, uni `Everywhere` ExMemoryUsage) => CekValue uni fun -> ExMemory
toExMemory = \case
    VCon c      -> memoryUsage c
    VDelay {}   -> 1
    VLamAbs {}  -> 1
    VBuiltin {} -> 1
{-# INLINE toExMemory #-}  -- It probably gets inlined anyway, but an explicit pragma
                           -- shouldn't hurt.

-- | A 'MonadError' version of 'try'.
tryError :: MonadError e m => m a -> m (Either e a)
tryError a = (Right <$> a) `catchError` (pure . Left)

runCekM
    :: forall a cost uni fun.
    (PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> Bool
    -> (forall s. GivenCekReqs uni fun s => CekM uni fun s a)
    -> (Either (CekEvaluationException uni fun) a, cost, [String])
runCekM (MachineParameters costs runtime) (ExBudgetMode getExBudgetInfo) emitting a = runST $ do
    exBudgetMode <- getExBudgetInfo
    mayLogsRef <- if emitting then Just <$> newSTRef DList.empty else pure Nothing
    let ?cekRuntime = runtime
        ?cekEmitter = mayLogsRef
        ?cekBudgetSpender = _exBudgetModeSpender exBudgetMode
        ?cekCosts = costs
        ?cekSlippage = defaultSlippage
    -- See Note Note [Being generic over 'term' in errors].
    errValOrErrTermOrRes <- unCekCarryingM . tryError . withTermErrors $ tryError @(CekEvaluationException uni fun) a
    let errOrRes = join $ first (mapCauseInMachineException (etermToTerm . dischargeCekValue)) errValOrErrTermOrRes
    st' <- _exBudgetModeGetFinal exBudgetMode
    logs <- case mayLogsRef of
        Nothing      -> pure []
        Just logsRef -> DList.toList <$> readSTRef logsRef
    pure (errOrRes, st', logs)

-- | Extend an environment with a variable name, the value the variable stands for
-- and the environment the value is defined in.
extendEnv :: Unique -> CekValue uni fun -> CekValEnv uni fun -> CekValEnv uni fun
extendEnv u = insertByUnique (coerce u)

-- | Look up a variable name in the environment.
lookupVarName :: forall uni fun s . (PrettyUni uni fun) => Unique -> CekValEnv uni fun -> CekM uni fun s (CekValue uni fun)
lookupVarName varName varEnv =
    case lookupUnique (coerce varName) varEnv of
        Nothing  -> throwingWithCause _MachineError OpenTermEvaluatedMachineError $ Just var where
            var = Var () (Name "" varName)
        Just val -> pure val

-- | Take pieces of a possibly partial builtin application and either create a 'CekValue' using
-- 'makeKnown' or a partial builtin application depending on whether the built-in function is
-- fully saturated or not.
evalBuiltinApp
    :: (GivenCekReqs uni fun s, PrettyUni uni fun)
    => fun
    -> ETerm uni fun
    -> CekValEnv uni fun
    -> BuiltinRuntime (CekValue uni fun)
    -> CekM uni fun s (CekValue uni fun)
evalBuiltinApp fun term env runtime@(BuiltinRuntime sch x cost) = case sch of
    TypeSchemeResult _ -> do
        spendBudgetCek (BBuiltinApp fun) cost
        flip unWithEmitterT emitCek $ makeKnown x
    _ -> pure $ VBuiltin fun term env runtime
{-# INLINE evalBuiltinApp #-}

-- See Note [Compilation peculiarities].
-- | The entering point to the CEK machine's engine.
enterComputeCek
    :: forall uni fun s
    . (Ix fun, PrettyUni uni fun, GivenCekReqs uni fun s, uni `Everywhere` ExMemoryUsage)
    => Context uni fun
    -> CekValEnv uni fun
    -> ETerm uni fun
    -> CekM uni fun s (ETerm uni fun)
enterComputeCek = computeCek (toWordArray 0) where
    -- | The computing part of the CEK machine.
    -- Either
    -- 1. adds a frame to the context and calls 'computeCek' ('Force', 'Apply')
    -- 2. calls 'returnCek' on values ('Delay', 'LamAbs', 'Constant', 'Builtin')
    -- 3. throws 'EvaluationFailure' ('Error')
    -- 4. looks up a variable in the environment and calls 'returnCek' ('Var')
    computeCek
        :: WordArray
        -> Context uni fun
        -> CekValEnv uni fun
        -> ETerm uni fun
        -> CekM uni fun s (ETerm uni fun)
    -- s ; ρ ▻ {L A}  ↦ s , {_ A} ; ρ ▻ L
    computeCek !unbudgetedSteps !ctx !env (EVar varName) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BVar unbudgetedSteps
        val <- lookupVarName varName env
        returnCek unbudgetedSteps' ctx val
    computeCek !unbudgetedSteps !ctx !_ (EConstant val) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BConst unbudgetedSteps
        returnCek unbudgetedSteps' ctx (VCon val)
    computeCek !unbudgetedSteps !ctx !env (ELamAbs name body) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BLamAbs unbudgetedSteps
        returnCek unbudgetedSteps' ctx (VLamAbs name body env)
    computeCek !unbudgetedSteps !ctx !env (EDelay body) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BDelay unbudgetedSteps
        returnCek unbudgetedSteps' ctx (VDelay body env)
    -- s ; ρ ▻ lam x L  ↦  s ◅ lam x (L , ρ)
    computeCek !unbudgetedSteps !ctx !env (EForce body) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BForce unbudgetedSteps
        computeCek unbudgetedSteps' (FrameForce ctx) env body
    -- s ; ρ ▻ [L M]  ↦  s , [_ (M,ρ)]  ; ρ ▻ L
    computeCek !unbudgetedSteps !ctx !env (EApply fun arg) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BApply unbudgetedSteps
        computeCek unbudgetedSteps' (FrameApplyArg env arg ctx) env fun
    -- s ; ρ ▻ abs α L  ↦  s ◅ abs α (L , ρ)
    -- s ; ρ ▻ con c  ↦  s ◅ con c
    -- s ; ρ ▻ builtin bn  ↦  s ◅ builtin bn arity arity [] [] ρ
    computeCek !unbudgetedSteps !ctx !env term@(EBuiltin bn) = do
        !unbudgetedSteps' <- stepAndMaybeSpend BBuiltin unbudgetedSteps
        meaning <- lookupBuiltin bn ?cekRuntime
        returnCek unbudgetedSteps' ctx (VBuiltin bn term env meaning)
    -- s ; ρ ▻ error A  ↦  <> A
    computeCek !_ !_ !_ EError =
        throwing_ _EvaluationFailure

    {- | The returning phase of the CEK machine.
    Returns 'EvaluationSuccess' in case the context is empty, otherwise pops up one frame
    from the context and uses it to decide how to proceed with the current value v.

      * 'FrameForce': call forceEvaluate
      * 'FrameApplyArg': call 'computeCek' over the context extended with 'FrameApplyFun'
      * 'FrameApplyFun': call 'applyEvaluate' to attempt to apply the function
          stored in the frame to an argument.
    -}
    returnCek
        :: WordArray
        -> Context uni fun
        -> CekValue uni fun
        -> CekM uni fun s (ETerm uni fun)
    --- Instantiate all the free variable of the resulting term in case there are any.
    -- . ◅ V           ↦  [] V
    returnCek !unbudgetedSteps NoFrame val = do
        spendAccumulatedBudget unbudgetedSteps
        pure $ dischargeCekValue val
    -- s , {_ A} ◅ abs α M  ↦  s ; ρ ▻ M [ α / A ]*
    returnCek !unbudgetedSteps (FrameForce ctx) fun = forceEvaluate unbudgetedSteps ctx fun
    -- s , [_ (M,ρ)] ◅ V  ↦  s , [V _] ; ρ ▻ M
    returnCek !unbudgetedSteps (FrameApplyArg argVarEnv arg ctx) fun =
        computeCek unbudgetedSteps (FrameApplyFun fun ctx) argVarEnv arg
    -- s , [(lam x (M,ρ)) _] ◅ V  ↦  s ; ρ [ x  ↦  V ] ▻ M
    -- FIXME: add rule for VBuiltin once it's in the specification.
    returnCek !unbudgetedSteps (FrameApplyFun fun ctx) arg =
        applyEvaluate unbudgetedSteps ctx fun arg

    -- | @force@ a term and proceed.
    -- If v is a delay then compute the body of v;
    -- if v is a builtin application then check that it's expecting a type argument,
    -- and either calculate the builtin application or stick a 'Force' on top of its 'Term'
    -- representation depending on whether the application is saturated or not,
    -- if v is anything else, fail.
    forceEvaluate
        :: WordArray
        -> Context uni fun
        -> CekValue uni fun
        -> CekM uni fun s (ETerm uni fun)
    forceEvaluate !unbudgetedSteps !ctx (VDelay body env) = computeCek unbudgetedSteps ctx env body
    forceEvaluate !unbudgetedSteps !ctx (VBuiltin fun term env (BuiltinRuntime sch f exF)) = do
        let term' = EForce term
        case sch of
            -- It's only possible to force a builtin application if the builtin expects a type
            -- argument next.
            TypeSchemeAll  _ schK -> do
                let runtime' = BuiltinRuntime (schK Proxy) f exF
                -- We allow a type argument to appear last in the type of a built-in function,
                -- otherwise we could just assemble a 'VBuiltin' without trying to evaluate the
                -- application.
                res <- evalBuiltinApp fun term' env runtime'
                returnCek unbudgetedSteps ctx res
            _ ->
                throwingWithCause _MachineError BuiltinTermArgumentExpectedMachineError (Just $ etermToTerm term')
    forceEvaluate !_ !_ val =
        throwingDischarged _MachineError NonPolymorphicInstantiationMachineError val

    -- | Apply a function to an argument and proceed.
    -- If the function is a lambda 'lam x ty body' then extend the environment with a binding of @v@
    -- to x@ and call 'computeCek' on the body.
    -- If the function is a builtin application then check that it's expecting a term argument,
    -- and either calculate the builtin application or stick a 'Apply' on top of its 'Term'
    -- representation depending on whether the application is saturated or not.
    -- If v is anything else, fail.
    applyEvaluate
        :: WordArray
        -> Context uni fun
        -> CekValue uni fun   -- lhs of application
        -> CekValue uni fun   -- rhs of application
        -> CekM uni fun s (ETerm uni fun)
    applyEvaluate !unbudgetedSteps !ctx (VLamAbs name body env) arg =
        computeCek unbudgetedSteps ctx (extendEnv name arg env) body
    -- Annotating @f@ and @exF@ with bangs gave us some speed-up, but only until we added a bang to
    -- 'VCon'. After that the bangs here were making things a tiny bit slower and so we removed them.
    applyEvaluate !unbudgetedSteps !ctx (VBuiltin fun term env (BuiltinRuntime sch f exF)) arg = do
        let term' = EApply term $ dischargeCekValue arg
        case sch of
            -- It's only possible to apply a builtin application if the builtin expects a term
            -- argument next.
            TypeSchemeArrow _ schB -> do
                -- The builtin application machinery wants to be able to throw a 'CekValue' rather
                -- than a 'Term', hence 'withCekValueErrors'.
                x <- withCekValueErrors $ readKnown arg
                -- TODO: should we bother computing that 'ExMemory' eagerly? We may not need it.
                -- We pattern match on @arg@ twice: in 'readKnown' and in 'toExMemory'.
                -- Maybe we could fuse the two?
                let runtime' = BuiltinRuntime schB (f x) . exF $ toExMemory arg
                res <- evalBuiltinApp fun term' env runtime'
                returnCek unbudgetedSteps ctx res
            _ ->
                throwingWithCause _MachineError UnexpectedBuiltinTermArgumentMachineError (Just $ etermToTerm term')
    applyEvaluate !_ !_ val _ =
        throwingDischarged _MachineError NonFunctionalApplicationMachineError val

    -- | Spend the budget that has been accumulated for a number of machine steps.
    spendAccumulatedBudget :: WordArray -> CekM uni fun s ()
    spendAccumulatedBudget !unbudgetedSteps = iforWordArray unbudgetedSteps spend

    -- Making this a definition of its own causes it to inline better than actually writing it inline, for
    -- some reason.
    -- Skip index 7, that's the total counter!
    -- See Note [Structure of the step counter]
    {-# INLINE spend #-}
    spend !i !w = unless (i == 7) $ let kind = toEnum i in spendBudgetCek (BStep kind) (stimes w (cekStepCost ?cekCosts kind))

    -- | Accumulate a step, and maybe spend the budget that has accumulated for a number of machine steps, but only if we've exceeded our slippage.
    stepAndMaybeSpend :: StepKind -> WordArray -> CekM uni fun s WordArray
    stepAndMaybeSpend !kind !unbudgetedSteps = do
        -- See Note [Structure of the step counter]
        let !ix = fromIntegral $ fromEnum kind
            !unbudgetedSteps' = overIndex 7 (+1) $ overIndex ix (+1) unbudgetedSteps
            !unbudgetedStepsTotal = readArray unbudgetedSteps' 7
        -- There's no risk of overflow here, since we only ever increment the total
        -- steps by 1 and then check this condition.
        if unbudgetedStepsTotal >= ?cekSlippage
        then spendAccumulatedBudget unbudgetedSteps' >> pure (toWordArray 0)
        else pure unbudgetedSteps'

-- See Note [Compilation peculiarities].
-- | Evaluate a term using the CEK machine and keep track of costing, logging is optional.
runCek
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> Bool
    -> ETerm uni fun
    -> (Either (CekEvaluationException uni fun) (ETerm uni fun), cost, [String])
runCek params mode emitting term =
    runCekM params mode emitting $ do
        spendBudgetCek BStartup (cekStartupCost ?cekCosts)
        enterComputeCek NoFrame mempty term
