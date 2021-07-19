{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module UntypedPlutusCore.Core.Type
    ( TPLC.UniOf
    , TPLC.Version (..)
    , TPLC.Binder (..)
    , Term (..)
    , Program (..)
    , toTerm
    , bindFunM
    , bindFun
    , mapFun
    , termAnn
    , erase
    , eraseProgram
    , ETerm (..)
    , EProgram (..)
    , termToETerm
    , etermToTerm
    ) where

import           Data.Functor.Identity
import           PlutusPrelude

import qualified PlutusCore.Constant                    as TPLC
import qualified PlutusCore.Core                        as TPLC
import           PlutusCore.Evaluation.Machine.ExMemory
import           PlutusCore.MkPlc
import qualified PlutusCore.Name                        as TPLC
import           Universe

{- Note [Term constructor ordering and numbers]
Ordering of constructors has a small but real effect on efficiency.
It's slightly more efficient to hit the earlier constructors, so it's better to put the more
common ones first.

The current ordering is based on their *empirically observed* frequency. We should check this
occasionally.

Additionally, the first 7 (or 3 on 32-bit systems) constructors will get *pointer tags*, which allows
more efficient access when casing on them. So we ideally want to keep the number of constructors
at 7 or fewer.

We've got 8 constructors, *but* the last one is Error, which is only going to be seen at most
once per program, so it's not too big a deal if it doesn't get a tag.
-}

{-| The type of Untyped Plutus Core terms. Mirrors the type of Typed Plutus Core terms except

1. all types are removed
2. 'IWrap' and 'Unwrap' are removed
3. type abstractions are replaced with 'Delay'
4. type instantiations are replaced with 'Force'

The latter two are due to the fact that we don't have value restriction in Typed Plutus Core
and hence a computation can be stuck expecting only a single type argument for the computation
to become unstuck. Therefore we can't just silently remove type abstractions and instantions and
need to replace them with something else that also blocks evaluation (in order for the semantics
of an erased program to match with the semantics of the original typed one). 'Delay' and 'Force'
serve exactly this purpose.
-}
-- Making all the fields strict gives us a couple of percent in benchmarks
-- See Note [Term constructor ordering and numbers]
data Term name uni fun ann
    = Var !ann !name
    | LamAbs !ann !name !(Term name uni fun ann)
    | Apply !ann !(Term name uni fun ann) !(Term name uni fun ann)
    | Force !ann !(Term name uni fun ann)
    | Delay !ann !(Term name uni fun ann)
    | Constant !ann !(Some (ValueOf uni))
    | Builtin !ann !fun
    -- This is the cutoff at which constructors won't get pointer tags
    -- See Note [Term constructor ordering and numbers]
    | Error !ann
    deriving stock (Show, Functor, Generic)
    deriving anyclass (NFData)

-- | A 'Program' is simply a 'Term' coupled with a 'Version' of the core language.
data Program name uni fun ann = Program ann (TPLC.Version ann) (Term name uni fun ann)
    deriving stock (Show, Functor, Generic)
    deriving anyclass (NFData)

type instance TPLC.UniOf (Term name uni fun ann) = uni

instance TermLike (Term name uni fun) TPLC.TyName name uni fun where
    var      = Var
    tyAbs    = \ann _ _ -> Delay ann
    lamAbs   = \ann name _ -> LamAbs ann name
    apply    = Apply
    constant = Constant
    builtin  = Builtin
    tyInst   = \ann term _ -> Force ann term
    unwrap   = const id
    iWrap    = \_ _ _ -> id
    error    = \ann _ -> Error ann

instance TPLC.AsConstant (Term name uni fun ann) where
    asConstant (Constant _ val) = pure val
    asConstant term             = TPLC.throwNotAConstant term

instance TPLC.FromConstant (Term name uni fun ()) where
    fromConstant = Constant ()

type instance TPLC.HasUniques (Term name uni fun ann) = TPLC.HasUnique name TPLC.TermUnique
type instance TPLC.HasUniques (Program name uni fun ann) = TPLC.HasUniques (Term name uni fun ann)

deriving via GenericExMemoryUsage (Term name uni fun ann) instance
    ( ExMemoryUsage name, ExMemoryUsage fun, ExMemoryUsage ann
    , Closed uni, uni `Everywhere` ExMemoryUsage
    ) => ExMemoryUsage (Term name uni fun ann)

toTerm :: Program name uni fun ann -> Term name uni fun ann
toTerm (Program _ _ term) = term

-- | Return the outermost annotation of a 'Term'.
termAnn :: Term name uni fun ann -> ann
termAnn (Constant ann _) = ann
termAnn (Builtin ann _)  = ann
termAnn (Var ann _)      = ann
termAnn (LamAbs ann _ _) = ann
termAnn (Apply ann _ _)  = ann
termAnn (Delay ann _)    = ann
termAnn (Force ann _)    = ann
termAnn (Error ann)      = ann

bindFunM
    :: Monad m
    => (ann -> fun -> m (Term name uni fun' ann))
    -> Term name uni fun ann
    -> m (Term name uni fun' ann)
bindFunM f = go where
    go (Constant ann val)     = pure $ Constant ann val
    go (Builtin ann fun)      = f ann fun
    go (Var ann name)         = pure $ Var ann name
    go (LamAbs ann name body) = LamAbs ann name <$> go body
    go (Apply ann fun arg)    = Apply ann <$> go fun <*> go arg
    go (Delay ann term)       = Delay ann <$> go term
    go (Force ann term)       = Force ann <$> go term
    go (Error ann)            = pure $ Error ann

bindFun
    :: (ann -> fun -> Term name uni fun' ann)
    -> Term name uni fun ann
    -> Term name uni fun' ann
bindFun f = runIdentity . bindFunM (coerce f)

mapFun :: (ann -> fun -> fun') -> Term name uni fun ann -> Term name uni fun' ann
mapFun f = bindFun $ \ann fun -> Builtin ann (f ann fun)

-- | Erase a Typed Plutus Core term to its untyped counterpart.
erase :: TPLC.Term tyname name uni fun ann -> Term name uni fun ann
erase (TPLC.Var ann name)           = Var ann name
erase (TPLC.TyAbs ann _ _ body)     = Delay ann (erase body)
erase (TPLC.LamAbs ann name _ body) = LamAbs ann name (erase body)
erase (TPLC.Apply ann fun arg)      = Apply ann (erase fun) (erase arg)
erase (TPLC.Constant ann con)       = Constant ann con
erase (TPLC.Builtin ann bn)         = Builtin ann bn
erase (TPLC.TyInst ann term _)      = Force ann (erase term)
erase (TPLC.Unwrap _ term)          = erase term
erase (TPLC.IWrap _ _ _ term)       = erase term
erase (TPLC.Error ann _)            = Error ann

-- | Erase a Typed Plutus Core Program to its untyped counterpart.
eraseProgram :: TPLC.Program tyname name uni fun ann -> Program name uni fun ann
eraseProgram (TPLC.Program a v t) = Program a v $ erase t

{-|
A specialized version of 'Term' for evaluation. The 'name' and 'a' parameters are specialized to 'Name' ('Unique') and '()' (omitted).
-}
data ETerm uni fun
    = EConstant !(Some (ValueOf uni))
    | EBuiltin !fun
    | EVar !TPLC.Unique
    | ELamAbs !TPLC.Unique !(ETerm uni fun)
    | EApply !(ETerm uni fun) !(ETerm uni fun)
    | EDelay !(ETerm uni fun)
    | EForce !(ETerm uni fun)
    | EError
    deriving stock (Show, Functor, Generic)
    deriving anyclass (NFData)

termToETerm :: Term TPLC.Name uni fun () -> ETerm uni fun
termToETerm = \case
    Constant _ c -> EConstant c
    Builtin _ f  -> EBuiltin f
    Var _ n      -> EVar (TPLC.nameUnique n)
    LamAbs _ n b -> ELamAbs (TPLC.nameUnique n) (termToETerm b)
    Apply _ l r  -> EApply (termToETerm l) (termToETerm r)
    Delay _ b    -> EDelay (termToETerm b)
    Force _ b    -> EForce (termToETerm b)
    Error _      -> EError

etermToTerm :: ETerm uni fun -> Term TPLC.Name uni fun ()
etermToTerm = \case
    EConstant c -> Constant () c
    EBuiltin f  -> Builtin () f
    EVar n      -> Var () (TPLC.Name "" n)
    ELamAbs n b -> LamAbs () (TPLC.Name "" n) (etermToTerm b)
    EApply l r  -> Apply () (etermToTerm l) (etermToTerm r)
    EDelay b    -> Delay () (etermToTerm b)
    EForce b    -> Force () (etermToTerm b)
    EError      -> Error ()

{-|
A specialized version of 'Program' for evaluation. The 'name' and 'a' parameters are specialized to 'Name' ('Unique') and '()' (omitted).
-}
data EProgram uni fun = EProgram (TPLC.Version ()) (ETerm uni fun)
    deriving stock (Show, Functor, Generic)
    deriving anyclass (NFData)
