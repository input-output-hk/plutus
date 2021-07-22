{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module PlutusIR.Purity (isPure) where

import           PlutusIR

import           PlutusCore.Constant.Meaning
import           PlutusCore.Constant.Typed

import           Data.Proxy

-- | An argument taken by a builtin: could be a term of a type.
data Arg tyname name uni fun a = TypeArg (Type tyname uni a) | TermArg (Term tyname name uni fun a)

-- | A (not necessarily saturated) builtin application, consisting of the builtin and the arguments it has been applied to.
data BuiltinApp tyname name uni fun a = BuiltinApp fun [Arg tyname name uni fun a]

saturatesScheme ::  [Arg tyname name uni fun a] -> TypeScheme term args res -> Maybe Bool
-- We've passed enough arguments that the builtin will reduce. Note that this also accepts over-applied builtins.
saturatesScheme _ TypeSchemeResult{}                       = Just True
-- Consume one argument
saturatesScheme (TermArg _ : args) (TypeSchemeArrow _ sch) = saturatesScheme args sch
saturatesScheme (TypeArg _ : args) (TypeSchemeAll _ k)     = saturatesScheme args (k Proxy)
-- Under-applied, not saturated
saturatesScheme [] TypeSchemeArrow{}                       = Just False
saturatesScheme [] TypeSchemeAll{}                         = Just False
-- These cases are only possible in case we have an ill-typed builtin application, so we can't give an answer.
saturatesScheme (TypeArg _ : _) TypeSchemeArrow{}          = Nothing
saturatesScheme (TermArg _ : _) TypeSchemeAll{}            = Nothing

-- | Is the given 'BuiltinApp' saturated? Returns 'Nothing' if something is badly wrong and we can't tell.
isSaturated
    :: forall tyname name uni fun a
    . ToBuiltinMeaning uni fun
    => BuiltinApp tyname name uni fun a
    -> Maybe Bool
isSaturated (BuiltinApp fun args) =
    case toBuiltinMeaning @uni @fun @(Term TyName Name uni fun ()) fun of
        BuiltinMeaning sch _ _ -> saturatesScheme args sch

-- | View a 'Term' as a 'BuiltinApp' if possible.
asBuiltinApp :: Term tyname name uni fun a -> Maybe (BuiltinApp tyname name uni fun a)
asBuiltinApp = undefined {- go []
    where
        go argsSoFar = \case
            Apply _ t arg  -> go (TermArg arg:argsSoFar) t
            TyInst _ t arg -> go (TypeArg arg:argsSoFar) t
            Builtin _ b    -> Just $ BuiltinApp b argsSoFar
            _              -> Nothing -}

{- Note [Purity, strictness, and variables]
Variables in PLC won't have effects: they can have something else substituted for them, but those will be fully evaluated already.
However, in PIR we have non-strict variable bindings. References to non-strict variables *can* have effects - after all,
they compile into an application.

So we need to take this into account in our purity calculation. We require the user to tell us which variables are strict, this
must be *conservative* (i.e. if you don't know, it's non-strict).
-}

-- | Will evaluating this term have side effects (looping or error)?. This is slightly wider than the definition of a value, as
-- it includes things that can't be returned from the machine (as they'd be ill-scoped).
isPure
    :: ToBuiltinMeaning uni fun
    => (name -> Strictness) -> Term tyname name uni fun a -> Bool
isPure varStrictness = go
    where
        go = \case
            -- See Note [Purity, strictness, and variables]
            Var _ n -> case varStrictness n of
                Strict    -> True
                NonStrict -> False
            -- These are syntactically values that won't reduce further
            LamAbs {} -> True
            TyAbs {} -> True
            Constant {} -> True
            IWrap _ _ _ t -> go t

            x | Just bapp@(BuiltinApp _ args) <- asBuiltinApp x ->
                -- Pure only if we can tell that the builtin application is not saturated
                (case isSaturated bapp of { Just b -> not b; Nothing -> False; })
                &&
                -- But all the arguments need to also be effect-free, since they will be evaluated
                -- when we evaluate the application.
                all (\case { TermArg arg -> go arg; TypeArg _ -> True }) args

            _ -> False
