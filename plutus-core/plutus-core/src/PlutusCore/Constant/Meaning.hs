-- GHC doesn't like the definition of 'makeBuiltinMeaning'.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE StandaloneKindSignatures  #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- DO NOT enable @StrictData@ in this file as it makes the evaluator slower (even with @~@ put in
-- 'BuiltinRuntime' in the places where it's necessary to have laziness for evaluators to work).

module PlutusCore.Constant.Meaning where

import           PlutusPrelude

import           PlutusCore.Constant.Dynamic.Emit
import           PlutusCore.Constant.Function
import           PlutusCore.Constant.Typed
import           PlutusCore.Core
import           PlutusCore.Data
import           PlutusCore.Evaluation.Machine.Exception
import           PlutusCore.Evaluation.Result
import           PlutusCore.Name

import           Control.Lens                            (ix, (^?))
import           Control.Monad.Except
import           Data.Array
import qualified Data.ByteString                         as BS
import qualified Data.Kind                               as GHC
import           Data.Proxy
import           Data.Some.GADT
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits

-- | The meaning of a built-in function consists of its type represented as a 'TypeScheme',
-- its Haskell denotation and a costing function (both in uninstantiated form).
--
-- The 'TypeScheme' of a built-in function is used for
--
-- 1. computing the PLC type of the function to be used during type checking
-- 2. checking equality of the expected type of an argument of a builtin and the actual one
--    during evaluation, so that we can avoid unsafe-coercing
--
-- A costing function for a built-in function is computed from a cost model for all built-in
-- functions from a certain set, hence the @cost@ parameter.
data BuiltinMeaning term cost =
    forall args res. BuiltinMeaning
        (TypeScheme term args res)
        (FoldArgs args res)
        (cost -> FoldArgsEx args)
-- I tried making it @(forall term. HasConstantIn uni term => TypeScheme term args res)@ instead of
-- @TypeScheme term args res@, but 'makeBuiltinMeaning' has to talk about
-- @KnownPolytype binds term args res a@ (note the @term@), because instances of 'KnownMonotype'
-- are constrained with @KnownType term arg@ and @KnownType term res@, and so the earliest we can
-- generalize from @term@ to @UniOf term@ is in 'toBuiltinMeaning'.
-- Besides, for 'BuiltinRuntime' we want to have a concrete 'TypeScheme' anyway for performance
-- reasons (there isn't much point in caching a value of a type with a constraint as it becomes a
-- function at runtime anyway, due to constraints being compiled as dictionaries).

-- TODO: we used to have arities and it was justified to precache them before executing an
-- evaluator, but now we need to reconsider that. Maybe instantiating 'BuiltinMeaning' on the fly
-- is in fact faster.
-- | A 'BuiltinRuntime' represents a possibly partial builtin application.
-- We get an initial 'BuiltinRuntime' representing an empty builtin application (i.e. just the
-- builtin with no arguments) by instantiating (via 'toBuiltinRuntime') a 'BuiltinMeaning'.
--
-- A 'BuiltinRuntime' contains info that is used during evaluation:
--
-- 1. the 'TypeScheme' of the uninstantiated part of the builtin. I.e. initially it's the type
--      scheme of the whole builtin, but applying or type-instantiating the builtin peels off
--      the corresponding constructor from the type scheme
-- 2. the (possibly partially instantiated) denotation
-- 3. the (possibly partially instantiated) costing function
--
-- All the three are in sync in terms of partial instantiatedness due to 'TypeScheme' being a
-- GADT and 'FoldArgs' and 'FoldArgsEx' operating on the index of that GADT.
data BuiltinRuntime term =
    forall args res. BuiltinRuntime
        (TypeScheme term args res)
        (FoldArgs args res)  -- Must be lazy, because we don't want to compute the denotation when
                             -- it's fully saturated before figuring out what it's going to cost.
        (FoldArgsEx args)    -- We make this lazy, so that evaluators that don't care about costing
                             -- can put @undefined@ here. TODO: we should test if making this strict
                             -- introduces any measurable speedup.

-- | A 'BuiltinRuntime' for each builtin from a set of builtins.
newtype BuiltinsRuntime fun term = BuiltinsRuntime
    { unBuiltinRuntime :: Array Int (BuiltinRuntime term)
    }

-- | Instantiate a 'BuiltinMeaning' given denotations of built-in functions and a cost model.
toBuiltinRuntime :: cost -> BuiltinMeaning term cost -> BuiltinRuntime term
toBuiltinRuntime cost (BuiltinMeaning sch f exF) = BuiltinRuntime sch f (exF cost)

-- | A type class for \"each function from a set of built-in functions has a 'BuiltinMeaning'\".
class (Bounded fun, Enum fun, Ix fun) => ToBuiltinMeaning uni fun where
    -- | The @cost@ part of 'BuiltinMeaning'.
    type CostingPart uni fun

    -- | Get the 'BuiltinMeaning' of a built-in function.
    toBuiltinMeaning :: HasConstantIn uni term => fun -> BuiltinMeaning term (CostingPart uni fun)

-- | Get the type of a built-in function.
typeOfBuiltinFunction :: ToBuiltinMeaning uni fun => fun -> Type TyName uni ()
typeOfBuiltinFunction fun = case toBuiltinMeaning @_ @_ @(Term TyName Name _ _ ()) fun of
    BuiltinMeaning sch _ _ -> typeSchemeToType sch

-- | Calculate runtime info for all built-in functions given denotations of builtins
-- and a cost model.
toBuiltinsRuntime
    :: forall term uni fun cost.
       (cost ~ CostingPart uni fun, HasConstantIn uni term, ToBuiltinMeaning uni fun)
    => cost -> BuiltinsRuntime fun term
toBuiltinsRuntime cost =
    BuiltinsRuntime . tabulateArraySub $ toBuiltinRuntime cost . toBuiltinMeaning @uni @fun

-- | Look up the runtime info of a built-in function during evaluation.
lookupBuiltin
    :: (MonadError (ErrorWithCause err term) m, AsMachineError err fun, Ix fun)
    => BuiltinTag fun -> BuiltinsRuntime fun val -> m (BuiltinRuntime val)
-- @Data.Array@ doesn't seem to have a safe version of @(!)@, hence we use a prism.
lookupBuiltin (BuiltinTag tag) (BuiltinsRuntime env) = case env ^? ix tag of
    Nothing  -> throwingWithCause _MachineError (UnknownBuiltin undefined) Nothing
    Just bri -> pure bri

{- Note [Automatic derivation of type schemes]
We use two type classes for automatic derivation of type schemes: 'KnownMonotype' and
'KnownPolytype'. The terminology is due to https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#The_Hindley%E2%80%93Milner_type_system

'KnownMonotype' and 'KnownPolytype' are responsible for deriving monomorphic and polymorphic types,
respectively. 'KnownMonotype' turns every argument that the Haskell denotation of a builtin
receives into a 'TypeSchemeArrow'. We extract the arguments from the type of the Haskell denotation
using the 'GetArgs' type family. 'KnownPolytype' turns every bound variable into a 'TypeSchemeAll'.
We extract variables from the type of the Haskell denotation using the 'ToBinds' type family
(in particular, see the @ToBinds (TypeScheme term args res)@ type instances). Variables are
collected in the order that they appear in (i.e. just like in Haskell). For example, processing
a type signature like

    const
        :: ( a ~ Opaque term (TyVarRep ('TyNameRep "a" 0))
           , b ~ Opaque term (TyVarRep ('TyNameRep "b" 1))
           )
        => b -> a -> b

with 'ToBinds' results in the following list of bindings:

    '[ 'Some ('TyNameRep "b" 1), 'Some ('TyNameRep "a" 0) ]

Higher-kinded type variables are fully supported.

The implementations of the 'KnownMonotype' and 'KnownPolytype' classes are structured in an
inference-friendly manner:

1. we compute @args@ using a type family ('GetArgs') in order to dispatch on the list of
   arguments of a built-in function in a way that doesn't force us to introduce overlapping
   instances
2. the @a -> res@ dependency allows us not to compute @res@ using a type family like with @args@
3. the @args res -> a@ dependency allows us not to mention @a@ in the type of 'knownMonotype'

Polymorphic built-in functions are handled via automatic specialization of all Haskell type
variables as types representing PLC type variables, as long as each Haskell variable appears as a
left argument to @(->)@ and is not buried somewhere inside (i.e. automatic derivation can handle
neither @f a@, @ListRep a@, nor @f Int@. Nor is @a -> b@ allowed to the left of an @(->)@.
Where all lower-case names are Haskell type variables). We'll call functions having such types
"simply-polymorphic". See the docs of 'EnumerateFromTo' for details.

The end result is that the user only has to specify the type of the denotation of a built-in
function and the 'TypeScheme' of the built-in function will be derived automatically. And in the
monomorphic and simply-polymorphic cases no types need to be specified at all.
-}

type family GetArgs a :: [GHC.Type] where
    GetArgs (a -> b) = a ': GetArgs b
    GetArgs _        = '[]

-- | A class that allows us to derive a monotype for a builtin.
class KnownMonotype term args res a | args res -> a, a -> res where
    knownMonotype :: TypeScheme term args res

-- | Once we've run out of term-level arguments, we return a 'TypeSchemeResult'.
instance (res ~ res', KnownType term res) => KnownMonotype term '[] res res' where
    knownMonotype = TypeSchemeResult Proxy

-- | Every term-level argument becomes as 'TypeSchemeArrow'.
instance (KnownType term arg, KnownMonotype term args res a) =>
            KnownMonotype term (arg ': args) res (arg -> a) where
    knownMonotype = Proxy `TypeSchemeArrow` knownMonotype

-- | Delete all @x@s from a list.
type family Delete x xs :: [a] where
    Delete _ '[]       = '[]
    Delete x (x ': xs) = Delete x xs
    Delete x (y ': xs) = y ': Delete x xs

-- | Delete all elements appearing in the first list from the second one and concatenate the lists.
type family Merge xs ys :: [a] where
    Merge '[]       ys = ys
    Merge (x ': xs) ys = x ': Delete x (Merge xs ys)

-- | Collect all unique variables (a variable consists of a textual name, a unique and a kind)
-- in an @x@.
type family ToBinds (x :: a) :: [Some TyNameRep]

type instance ToBinds '[]       = '[]
type instance ToBinds (x ': xs) = Merge (ToBinds x) (ToBinds xs)

type instance ToBinds Integer       = '[]
type instance ToBinds BS.ByteString = '[]
type instance ToBinds Char          = '[]
type instance ToBinds ()            = '[]
type instance ToBinds Bool          = '[]
type instance ToBinds Int           = '[]
type instance ToBinds Data          = '[]
type instance ToBinds []            = '[]
type instance ToBinds (,)           = '[]
type instance ToBinds [a]           = '[]  -- One can't directly put a PLC type variable into lists
type instance ToBinds (a, b)        = '[]  -- or tuples ('SomeConstantOf' has to be used for that),
                                           -- hence we say that polymorphic built-in types can't
                                           -- directly contain any PLC type variables in them.

type instance ToBinds (EvaluationResult a)      = ToBinds a
type instance ToBinds (Emitter a)               = ToBinds a
type instance ToBinds (Opaque _ rep)            = ToBinds rep
type instance ToBinds (SomeConstant _ rep)      = ToBinds rep
type instance ToBinds (SomeConstantOf _ _ reps) = ToBinds reps

type instance ToBinds (TyVarRep var) = '[ 'Some var ]
type instance ToBinds (TyAppRep fun arg) = Merge (ToBinds fun) (ToBinds arg)
type instance ToBinds (TyForallRep var a) = Delete ('Some var) (ToBinds a)

type instance ToBinds (TypeScheme term '[]           res) = ToBinds res
type instance ToBinds (TypeScheme term (arg ': args) res) =
    Merge (ToBinds arg) (ToBinds (TypeScheme term args res))

-- | A class that allows us to derive a polytype for a builtin.
class KnownPolytype (binds :: [Some TyNameRep]) term args res a | args res -> a, a -> res where
    knownPolytype :: Proxy binds -> TypeScheme term args res

-- | Once we've run out of type-level arguments, we start handling term-level ones.
instance KnownMonotype term args res a => KnownPolytype '[] term args res a where
    knownPolytype _ = knownMonotype

-- Here we unpack an existentially packed @kind@ and constrain it afterwards!
-- So promoted existentials are true sigmas! If we were at the term level, we'd have to pack
-- @kind@ along with the @KnownKind kind@ constraint, otherwise when we unpack the existential,
-- all information is lost and we can't do anything with @kind@.
-- | Every type-level argument becomes a 'TypeSchemeAll'.
instance (KnownSymbol name, KnownNat uniq, KnownKind kind, KnownPolytype binds term args res a) =>
            KnownPolytype ('Some ('TyNameRep @kind name uniq) ': binds) term args res a where
    knownPolytype _ = TypeSchemeAll @name @uniq @kind Proxy $ \_ -> knownPolytype (Proxy @binds)

-- The 'TryUnify' gadget explained in detail in https://github.com/effectfully/sketches/tree/master/poly-type-of-saga/part1-try-unify

-- | Check if two values of different kinds are in fact the same value (with the same kind).
-- A heterogeneous version of @Type.Equality.(==)@.
type (===) :: forall a b. a -> b -> Bool
type family x === y where
    x === x = 'True
    x === y = 'False

type TryUnify :: forall a b. Bool -> a -> b -> GHC.Constraint
class same ~ (x === y) => TryUnify same x y
instance (x === y) ~ 'False => TryUnify 'False x y
instance {-# INCOHERENT #-} (x ~~ y, same ~ 'True) => TryUnify same x y

-- | Unify two values unless they're obviously distinct (in which case do nothing).
-- Allows us to detect and specialize type variables, since a type variable is not obviously
-- distinct from anything else and so the INCOHERENT instance of 'TryUnify' gets triggered and the
-- variable gets unified with whatever we want it to.
type (~?~) :: forall a b. a -> b -> GHC.Constraint
type x ~?~ y = TryUnify (x === y) x y

-- | Get the element at an @i@th position in a list.
type Lookup :: forall a. Nat -> [a] -> a
type family Lookup n xs where
    Lookup 0 (x ': xs) = x
    Lookup n (_ ': xs) = Lookup (n - 1) xs

-- | Get the name at the @i@th position in the list of default names. We could use @a_0@, @a_1@,
-- @a_2@ etc instead, but @a@, @b@, @c@ etc are nicer.
type GetName i = Lookup i '["a", "b", "c", "d", "e", "f", "g", "h"]

-- | Try to specialize @a@ as a type representing a Plutus type variable.
-- @i@ is a fresh id and @j@ is a final one (either @i + 1@ or @i@ depending on whether
-- specialization attempt is successful or not).
type TrySpecializeAsVar :: Nat -> Nat -> GHC.Type -> GHC.Type -> GHC.Constraint
class TrySpecializeAsVar i j term a | i term a -> j
instance
    ( var ~ Opaque term (TyVarRep ('TyNameRep (GetName i) i))
    -- Try to unify @a@ with a freshly created @var@.
    , a ~?~ var
    -- If @a@ is equal to @var@ then unification was successful and we just used the fresh id and
    -- so we need to bump it up. Otherwise @var@ was discarded and so the fresh id is still fresh.
    -- Replacing @(===)@ with @(==)@ causes errors at use site, for whatever reason.
    , j ~ If (a === var) (i + 1) i
    ) => TrySpecializeAsVar i j term a

-- | For looking into the type of a constant or the type arguments of a polymorphic built-in type
-- and specializing them as types representing Plutus type variables.
-- @i@ is a fresh id and @j@ is a final one as in 'TrySpecializeAsVar', but since 'HandleSomeConstant'
-- can specialize multiple variables, @j@ can be equal to @i + n@ for any @n@ (including @0@).
type HandleSomeConstant :: Nat -> Nat -> GHC.Type -> GHC.Type -> GHC.Constraint
class HandleSomeConstant i j term a | i term a -> j
instance {-# OVERLAPPABLE #-} i ~ j => HandleSomeConstant i j term a
-- Take an argument of a built-in type and try to specialize it as a type representing a Plutus
-- type variable. Note that we don't explicitly handle the no-more-arguments case as it's handled
-- by the OVERLAPPABLE instance right above.
instance {-# OVERLAPPING #-}
    ( TrySpecializeAsVar i j term rep
    , HandleSomeConstant j k term (SomeConstantOf uni f reps)
    ) => HandleSomeConstant i k term (SomeConstantOf uni f (rep ': reps))
instance {-# OVERLAPPING #-}
    ( TrySpecializeAsVar i j term rep
    ) => HandleSomeConstant i j term (SomeConstant uni rep)

-- See https://github.com/effectfully/sketches/tree/master/poly-type-of-saga/part2-enumerate-type-vars
-- for a detailed elaboration on how this works.
-- | Specialize each Haskell type variable in @a@ as a type representing a Plutus Core type variable
-- by deconstructing @a@ into an applied @(->)@ (we don't recurse to the left of @(->)@, only to the
-- right) and trying to specialize every argument type as a PLC type variable
-- (via 'TrySpecializeAsVar') until no deconstruction is possible, at which point we've got a result
-- which we don't try to specialize, because that would require an incoherent instance and
-- introducing one makes code that otherwise type checks perfectly throw errors due to a bug in GHC,
-- see https://github.com/input-output-hk/plutus/pull/2521#issuecomment-759522445
-- In practice this means that if the result is a type variable, then this type variable has to
-- be mentioned as an argument type for inference to work. I.e. @absurd :: Void -> a@ does not get
-- inferred, since @a@ is only mentioned as the result type and not as an argument type.
-- But that's a fairly rear use case and we can always provide a type signature manually.
type EnumerateFromTo :: Nat -> Nat -> GHC.Type -> GHC.Type -> GHC.Constraint
class EnumerateFromTo i j term a | i term a -> j
instance {-# OVERLAPPABLE #-} i ~ j => EnumerateFromTo i j term a
instance {-# OVERLAPPING #-}
    ( TrySpecializeAsVar i j term a
    , HandleSomeConstant j k term a
    , EnumerateFromTo k l term b
    ) => EnumerateFromTo i l term (a -> b)

-- See Note [Automatic derivation of type schemes]
-- | Construct the meaning for a built-in function by automatically deriving its
-- 'TypeScheme', given
--
-- 1. the denotation of the builtin
-- 2. an uninstantiated costing function
makeBuiltinMeaning
    :: forall a term cost binds args res j.
       ( args ~ GetArgs a, a ~ FoldArgs args res, binds ~ ToBinds (TypeScheme term args res)
       , KnownPolytype binds term args res a, EnumerateFromTo 0 j term a
       )
    => a -> (cost -> FoldArgsEx args) -> BuiltinMeaning term cost
makeBuiltinMeaning = BuiltinMeaning (knownPolytype (Proxy @binds) :: TypeScheme term args res)
