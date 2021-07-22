{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module UntypedPlutusCore.Evaluation.HOAS
    ( HTerm
    , UserHoasError (..)
    , InternalHoasError (..)
    , HoasException
    , Value
    , BuiltinApp
    , EvalM
    , evaluateHoas
    , unsafeEvaluateHoas
    ) where

import           UntypedPlutusCore.Core                  (BuiltinTag (..), Term (..), UniOf, bindFunM)

import           PlutusCore.Constant                     hiding (lookupBuiltin)
import           PlutusCore.Evaluation.Machine.Exception
import           PlutusCore.Evaluation.Result
import           PlutusCore.Name
import           PlutusCore.Pretty

import           Control.Lens                            (ix, (^?))
import           Control.Lens.TH
import           Control.Monad.Except
import           Data.Array
import           Data.Bifunctor
import           Data.Proxy
import           Data.Typeable
import           Universe

-- | A higher-order version of 'Term'.
-- We parameterize it by a monad, because there's no way we could generally convert a first-order
-- 'Term' into a higher-order 'HTerm' in a pure way: the original term may contain free variables
-- and we can't look under lambdas to find that out before evaluation starts. Hence we need to
-- postpone converting variables when they appear under lambdas, which either means throwing an
-- 'error' on a free variable or embedding delayed monadic computations right into the AST.
-- It's not an unseen trick, here Kmett uses, for example: https://www.reddit.com/r/haskell/comments/j2q5p8/monthly_hask_anything_october_2020/g7zunsk/
-- Except he hardcodes the monad to be 'IO' and we keep it general, which seems convenient.
data HTerm m name uni fun ann
    = HConstant ann (Some (ValueOf uni))
    | HBuiltin ann fun
    | HVar ann name
    | HLamAbs ann name (HTerm m name uni fun ann -> m (HTerm m name uni fun ann))
    | HApply ann (HTerm m name uni fun ann) (HTerm m name uni fun ann)
    -- @(->) ()@ is to make sure the body of a 'Delay' does not get evaluated before the 'Delay'
    -- is forced. Laziness would already save us, but it's worth being explicit, hence the dummy
    -- argument.
    | HDelay ann (() -> m (HTerm m name uni fun ann))
    | HForce ann (HTerm m name uni fun ann)
    | HError ann

type instance UniOf (HTerm m name uni fun ann) = uni

instance AsConstant (HTerm m name uni fun ann) where
    asConstant (HConstant _ val) = pure val
    asConstant term              = throwNotAConstant term

instance FromConstant (HTerm m name uni fun ()) where
    fromConstant = HConstant ()

data UserHoasError
    = HoasEvaluationFailure
    deriving (Show, Eq)

-- Those perhaps shouldn't be called "internal". The user can easily trigger the first three by
-- uploading the wrong thing. But then we have the same issue with the CEK machine where
-- 'MachineError' is not that internal as well.
data InternalHoasError fun
    = FreeVariableHoasError
    | UnliftingHoasError UnliftingError
    | ArityHoasError
    | UnknownBuiltinHoasError fun
    deriving (Show, Eq, Functor)

instance PrettyBy PrettyConfigPlc UserHoasError where
    prettyBy _ HoasEvaluationFailure = "EvaluationFailure"

instance Pretty fun => PrettyBy config (InternalHoasError fun) where
    prettyBy _ = pretty . show . fmap (display @String)

-- | The type of errors that can occur during HOAS-based evaluation.
type HoasException fun = EvaluationException UserHoasError (InternalHoasError fun)

instance AsEvaluationFailure UserHoasError where
    _EvaluationFailure = _EvaluationFailureVia HoasEvaluationFailure

-- | A 'Value' is an 'HTerm' being evaluatated in the 'EvalM' monad and with built-in functions
-- mapped to their (possibly partially applied) meanings.
type Value unique name uni fun ann =
    HTerm (EvalM unique name uni fun ann) name uni (BuiltinApp unique name uni fun ann) ann

-- See Note [Builtin application evaluation].
-- | A builtin application consists of a (possibly partially applied) built-in function and
-- a delayed computation returning the 'Term' representation of that builtin, which we need
-- in case the built-in function never gets fully saturated, which requires us to put the
-- (possibly partially applied) builtin into the resulting term.
data BuiltinApp unique name uni fun ann = BuiltinApp
    { _builtinAppTerm    :: EvalM unique name uni fun ann (Term name uni fun ann)
    , _builtinAppRuntime :: BuiltinRuntime (Value unique name uni fun ann)
    }

-- 'EvalM' is referenced in 'Value', so 'EvalM' is recursive and hence it has to be a @newtype@.
-- | The monad the HOAS evaluator runs in.
newtype EvalM unique name uni fun ann a = EvalM
    { unEvalM :: Either (HoasException fun (Value unique name uni fun ann)) a
    } deriving newtype
        ( Functor, Applicative, Monad
        , MonadError (HoasException fun (Value unique name uni fun ann))
        )
      -- No logging for now.
      deriving (MonadEmitter) via (NoEmitterT (EvalM unique name uni fun ann))

makeClassyPrisms ''UserHoasError
makeClassyPrisms ''InternalHoasError

instance AsInternalHoasError internal fun =>
            AsInternalHoasError (EvaluationError user internal) fun where
    _InternalHoasError = _InternalEvaluationError . _InternalHoasError
instance AsUnliftingError (InternalHoasError fun) where
    _UnliftingError = _UnliftingHoasError

-- | Convert an 'HTerm' into a 'Term' running all internal monadic actions along the way.
fromHTerm :: Monad m => HTerm m name uni fun ann -> m (Term name uni fun ann)
fromHTerm (HConstant ann val)     = pure $ Constant ann val
fromHTerm (HBuiltin ann fun)      = undefined -- pure $ Builtin ann fun
fromHTerm (HVar ann name)         = pure $ Var ann name
-- Here we do not recover the original annotation and instead use the one that the whole lambda
-- is annotated with. We could probably handle annotations better, but we don't care for now.
-- BUG: feeding @HVar ann name@ to @body@ can easily result in variable capture.
fromHTerm (HLamAbs ann name body) = LamAbs ann name <$> (body (HVar ann name) >>= fromHTerm)
fromHTerm (HApply ann fun arg)    = Apply ann <$> fromHTerm fun <*> fromHTerm arg
fromHTerm (HDelay ann getBody)    = Delay ann <$> (getBody () >>= fromHTerm)
fromHTerm (HForce ann term)       = Force ann <$> fromHTerm term
fromHTerm (HError ann)            = pure $ Error ann

-- | Convert a 'Value' into a 'Term' running all internal monadic actions along the way and
-- extracting all partial builtin applications.
fromValue
    :: Value unique name uni fun ann
    -> EvalM unique name uni fun ann (Term name uni fun ann)
fromValue = fromHTerm >=> bindFunM (const _builtinAppTerm)

-- | Run an 'EvalM' computation and convert the cause of a possible error from 'Value' to 'Term'.
runEvalM
    :: EvalM unique name uni fun ann a
    -> Either (HoasException fun (Term name uni fun ann)) a
runEvalM = first errorValueToTerm . unEvalM where
    -- Here we call 'runEvalM' recursively. It's fine when the underlying monad is 'Either',
    -- but if it had 'ReaderT', then we'd also need to make sure that 'runEvalM' is supplied
    -- with the most recent environment, not the initial one.
    errorValueToTerm = either id id . traverse (runEvalM . fromValue)

lookupVar
    :: HasUnique name unique
    => ann -> name -> UniqueMap unique value -> EvalM unique name uni fun ann value
lookupVar ann name env =
    case lookupName name env of
        Just term -> pure term
        Nothing   ->
            throwingWithCause _InternalHoasError FreeVariableHoasError $
                Just $ HVar ann name

-- | Retrieve the meaning of a built-in function.
lookupBuiltin
    :: (value ~ Value unique name uni fun ann, Ix fun)
    => ann -> BuiltinTag fun -> BuiltinsRuntime fun value -> EvalM unique name uni fun ann value
lookupBuiltin ann fun (BuiltinsRuntime meanings) = undefined
    -- case meanings ^? ix fun of
    --     Nothing      -> throwingWithCause _InternalHoasError (UnknownBuiltinHoasError fun) Nothing
    --     Just meaning -> pure . HBuiltin ann $ BuiltinApp (pure $ Builtin ann fun) meaning

{- Note [Handling non-constant results]
Evaluation may return a non-constant term. This has a couple of implications:

1. we have to keep a 'Term' representation of a partial builtin application, so that if evaluation
   results in an underapplied builtin like @addinteger 1@, we can return that term. If we were only
   to keep the denotation of a builtin, we couldn't reconstruct the term from it
2. 'evalApply' and 'evalForce' have to handle the case when their argument is neither a
   'HLamAbs'/'HDelay' nor a built-in function, because if evaluation results in, say,
   @\f -> f 1@, then we need to turn that application into an 'HApply' AST node (which itself
   can be forced or applied to another term), which upon final reification becomes an 'Apply' node.
   This is the usual ways HOAS evaluation works
-}

{- Note [Builtin application evaluation]
The HOAS evaluator uses a different way to evaluate builtin applications. Instead of collecting
arguments in a list like the CEK machine does, we store partially applied meanings of builtins
right in the AST by instantiating the @fun@ type variable to 'BuiltinApp'. This allows us to
feed a builtin as soon as an 'Apply' or a 'Force' pops up. The builtin application gets evaluated
one it's fully saturated, before that it's just feeding arguments one by one to the denotation
of the builtin and collecting a 'Term' version of the application
(see Note [Handling non-constant results]).
-}

-- | Take pieces of a 'BuiltinApp' and either create a 'Value' using 'makeKnown' or a partial
-- builtin application depending on whether the built-in function is fully saturated or not.
evalBuiltinApp
    :: ann
    -> EvalM unique name uni fun ann (Term name uni fun ann)
    -> BuiltinRuntime (Value unique name uni fun ann)
    -> EvalM unique name uni fun ann (Value unique name uni fun ann)
-- Note the absence of 'evalValue'. Same logic as with the CEK machine applies:
-- 'makeKnown' never returns a non-value term.
evalBuiltinApp _   _       (BuiltinRuntime (TypeSchemeResult _) x _) = makeKnown x
evalBuiltinApp ann getTerm runtime =
    pure . HBuiltin ann $ BuiltinApp getTerm runtime

-- See Note [Builtin application evaluation].
-- | Either 'Apply' or 'Force' a (possibly partially applied) built-in function depending on
--
-- 1. what the builtin expects
-- 2. whether the 'Maybe' argument is a 'Just' or 'Nothing'
--
-- (the two answers must agree, otherwise we have an error) and invoke 'evalBuiltinApp'.
evalFeedBuiltinApp
    :: ann
    -> BuiltinApp unique name uni fun ann
    -> Maybe (Value unique name uni fun ann)
    -> EvalM unique name uni fun ann (Value unique name uni fun ann)
evalFeedBuiltinApp ann (BuiltinApp getTerm (BuiltinRuntime sch f _)) e =
    case (sch, e) of
        (TypeSchemeArrow _ schB, Just arg) -> do
            x <- readKnown arg
            evalBuiltinApp
                ann
                (Apply ann <$> getTerm <*> fromValue arg)
                (BuiltinRuntime schB (f x) noCosting)
        (TypeSchemeAll  _ schK, Nothing) ->
            evalBuiltinApp
                ann
                (Force ann <$> getTerm)
                (BuiltinRuntime (schK Proxy) f noCosting)
        _ ->
            throwingWithCause _InternalHoasError ArityHoasError Nothing
  where
    -- I guess we could use a no-costing version of 'BuiltinRuntime', but I prefer to reuse
    -- the existing one even if it means throwing an error on any attempt to do something
    -- involving costs. Especially since it's planned to support costing in the HOAS evaluator.
    noCosting = error "HOAS currently does not support costing"

-- See Note [Handling non-constant results].
-- | Evaluate the application of a function to a value.
evalApply
    :: ann
    -> Value unique name uni fun ann
    -> Value unique name uni fun ann
    -> EvalM unique name uni fun ann (Value unique name uni fun ann)
evalApply _   (HLamAbs _ _ body) arg = body arg
evalApply _   (HBuiltin ann fun) arg = evalFeedBuiltinApp ann fun $ Just arg
evalApply ann fun                arg = pure $ HApply ann fun arg

-- See Note [Handling non-constant results].
-- | Force a delayed computation.
evalForce
    :: ann
    -> Value unique name uni fun ann
    -> EvalM unique name uni fun ann (Value unique name uni fun ann)
evalForce _   (HDelay _ getBody) = getBody ()
evalForce _   (HBuiltin ann fun) = evalFeedBuiltinApp ann fun Nothing
evalForce ann term               = pure $ HForce ann term

-- See Note [Builtin application evaluation]
-- | Evaluate a 'Term' in the 'EvalM' monad using HOAS.
evalTerm
    :: forall ann fun uni name unique term value evalM.
       ( term ~ Term name uni fun ann, value ~ Value unique name uni fun ann
       , evalM ~ EvalM unique name uni fun ann, HasUnique name unique, Ix fun
       )
    => BuiltinsRuntime fun value -> term -> evalM value
evalTerm runtime = go mempty where
    go :: UniqueMap unique value -> term -> evalM value
    go _   (Constant ann val) = pure $ HConstant ann val
    -- Using 'evalBuiltinApp' here would allow us to have named constants as builtins.
    -- Not that this is supported by anything else, though.
    go _   (Builtin ann fun) = lookupBuiltin ann fun runtime
    go env (Var ann name) = lookupVar ann name env
    go env (LamAbs ann name body) =
        pure . HLamAbs ann name $ \arg -> go (insertByName name arg env) body
    go env (Apply ann fun arg) = do
        fun' <- go env fun
        arg' <- go env arg
        evalApply ann fun' arg'
    go env (Delay ann term) = pure . HDelay ann $ \() -> go env term
    go env (Force ann term) = go env term >>= evalForce ann
    go _   (Error ann) = throwingWithCause _EvaluationFailure () . Just $ HError ann

-- | Evaluate a term using the HOAS evaluator.
evaluateHoas
    :: ( term ~ Term name uni fun ann, value ~ Value unique name uni fun ann
       , HasUnique name unique, Ix fun
       )
    => BuiltinsRuntime fun value -> term -> Either (HoasException fun term) term
evaluateHoas runtime term = runEvalM $ evalTerm runtime term >>= fromValue

-- | Evaluate a term using the HOAS evaluator. May throw a 'HoasException'.
unsafeEvaluateHoas
    :: ( term ~ Term name uni fun ann, value ~ Value unique name uni fun ann
       , HasUnique name unique, Ix fun
       , Typeable name, Typeable uni, Typeable fun, Typeable ann
       , Pretty fun, PrettyPlc term
       )
    => BuiltinsRuntime fun value -> term -> EvaluationResult term
unsafeEvaluateHoas runtime = unsafeExtractEvaluationResult . evaluateHoas runtime
