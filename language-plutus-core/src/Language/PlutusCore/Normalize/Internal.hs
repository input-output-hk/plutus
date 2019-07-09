-- | The internals of the normalizer.

-- Due to the generated 'normalizeEnvCountStep' below which is not used.
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.PlutusCore.Normalize.Internal
    ( NormalizeTypeT
    , runNormalizeTypeM
    , runNormalizeTypeFullM
    , runNormalizeTypeGasM
    , withExtendedTypeVarEnv
    , normalizeTypeM
    , substNormalizeTypeM
    , normalizeTypesInM
    ) where

import           Language.PlutusCore.Name
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Rename
import           Language.PlutusCore.Type
import           PlutusPrelude

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

{- Note [Global uniqueness]
WARNING: everything in this module works under the assumption that the global uniqueness condition
is satisfied. The invariant is not checked, enforced or automatically fulfilled. So you must ensure
that the global uniqueness condition is satisfied before calling ANY function from this module.

The invariant is preserved. In future we will enforce the invariant.
-}

-- | Mapping from variables to what they stand for (each row represents a substitution).
-- Needed for efficiency reasons, otherwise we could just use substitutions.
type TypeVarEnv tyname uni ann =
    UniqueMap TypeUnique (Dupable (Normalized (Type tyname uni ann)))

-- | The environments that type normalization runs in.
data NormalizeTypeEnv m tyname uni ann = NormalizeTypeEnv
    { _normalizeTypeEnvTypeVarEnv :: TypeVarEnv tyname uni ann
    , _normalizeTypeEnvCountStep  :: m ()
      -- ^ How to count a type normalization step.
    }

makeLenses ''NormalizeTypeEnv

{- Note [NormalizeTypeT]
Type normalization requires 'Quote' (because we need to be able to generate fresh names), but we
do not put 'Quote' into 'NormalizeTypeT'. The reason for this is that it makes type signatures of
various runners much nicer and also more generic. For example, we have

    runNormalizeTypeFullM :: MonadQuote m => NormalizeTypeT m tyname uni ann a -> m a

If 'NormalizeTypeT' contained 'Quote', it would be

    runNormalizeTypeFullM :: NormalizeTypeT m tyname uni ann a -> QuoteT m a

which hardcodes 'QuoteT' to be the outermost transformer.

Type normalization can run in any @m@ (as long as it's a 'MonadQuote') as witnessed by
the following type signature:

    normalizeTypeM
        :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
        => Type tyname uni ann -> NormalizeTypeT m tyname uni ann (Normalized (Type tyname uni ann))

so it's natural to have runners that do not break this genericity.
-}

{- Note [Normalization API]
Normalization is split in two parts:

1. functions returning computations that perform reductions and run in defined in this module
   monad transformers (e.g. 'NormalizeTypeT')
2. runners of those computations

The reason for splitting the API is that this way the type-theoretic notion of normalization is
separated from implementation-specific details like how to count gas (we hardcode *where* to count
gas, but this can be generalized in case we need it). And this is important, because gas counting
requires access to different monads in different scenarios, so in the end we have a fine-grained API
instead of a single function that reflects all possible effects from distinct scenarios in its type
signature.
-}

-- See Note [NormalizedTypeT].
-- | The monad transformer that type normalization runs in.
newtype NormalizeTypeT m tyname uni ann a = NormalizeTypeT
    { unNormalizeTypeT :: ReaderT (NormalizeTypeEnv m tyname uni ann) m a
    } deriving newtype
        ( Functor, Applicative, Alternative, Monad, MonadPlus
        , MonadReader (NormalizeTypeEnv m tyname uni ann), MonadState s
        , MonadQuote
        )

-- | Run a 'NormalizeTypeM' computation.
runNormalizeTypeM :: m () -> NormalizeTypeT m tyname uni ann a -> m a
runNormalizeTypeM countStep (NormalizeTypeT a) =
    runReaderT a $ NormalizeTypeEnv mempty countStep

-- | Run a 'NormalizeTypeM' computation without dealing with gas.
runNormalizeTypeFullM
    :: MonadQuote m => NormalizeTypeT m tyname uni ann a -> m a
runNormalizeTypeFullM = runNormalizeTypeM $ pure ()

-- | Run a gas-consuming 'NormalizeTypeM' computation.
-- Count a single substitution step by subtracting @1@ from available gas or
-- fail when there is no available gas.
runNormalizeTypeGasM
    :: MonadQuote m => Gas -> NormalizeTypeT (StateT Gas (MaybeT m)) tyname uni ann a -> m (Maybe a)
runNormalizeTypeGasM gas a = runMaybeT $ evalStateT (runNormalizeTypeM countSubst a) gas where
    countSubst = do
        Gas gas' <- get
        if gas' == 0
            then mzero
            else put . Gas $ gas' - 1

countTypeNormalizationStep :: NormalizeTypeT m tyname uni ann ()
countTypeNormalizationStep = NormalizeTypeT $ ReaderT _normalizeTypeEnvCountStep

-- | Locally extend a 'TypeVarEnv' in a 'NormalizeTypeM' computation.
withExtendedTypeVarEnv
    :: (HasUnique (tyname ann) TypeUnique, Monad m)
    => tyname ann
    -> Normalized (Type tyname uni ann)
    -> NormalizeTypeT m tyname uni ann a
    -> NormalizeTypeT m tyname uni ann a
withExtendedTypeVarEnv name =
    local . over normalizeTypeEnvTypeVarEnv . insertByName name . pure

-- | Look up a @tyname@ in a 'TypeVarEnv'.
lookupTyNameM
    :: (HasUnique (tyname ann) TypeUnique, Monad m)
    => tyname ann -> NormalizeTypeT m tyname uni ann (Maybe (Dupable (Normalized (Type tyname uni ann))))
lookupTyNameM name = asks $ lookupName name . _normalizeTypeEnvTypeVarEnv

{- Note [Normalization]
Normalization works under the assumption that variables are globally unique.
We use environments instead of substitutions as they're more efficient.

Since all names are unique and there is no need to track scopes, type normalization has only two
interesting cases: function application and a variable usage. In the function application case we
normalize a function and its argument, add the normalized argument to the environment and continue
normalization. In the variable case we look up the variable in the current environment: if it's not
found, we leave the variable untouched. If the variable is found, then what this variable stands for
was previously added to an environment (while handling the function application case), so we pick
this value and rename all bound variables in it to preserve the global uniqueness condition. It is
safe to do so, because picked values cannot contain uninstantiated variables as only normalized types
are added to environments and normalization instantiates all variables presented in an environment.
-}

-- See Note [Normalization].
-- | Normalize a 'Type' in the 'NormalizeTypeM' monad.
normalizeTypeM
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => Type tyname uni ann -> NormalizeTypeT m tyname uni ann (Normalized (Type tyname uni ann))
normalizeTypeM (TyForall ann name kind body) =
    TyForall ann name kind <<$>> normalizeTypeM body
normalizeTypeM (TyIFix ann pat arg)          =
    TyIFix ann <<$>> normalizeTypeM pat <<*>> normalizeTypeM arg
normalizeTypeM (TyFun ann dom cod)           =
    TyFun ann <<$>> normalizeTypeM dom <<*>> normalizeTypeM cod
normalizeTypeM (TyLam ann name kind body)    =
    TyLam ann name kind <<$>> normalizeTypeM body
normalizeTypeM (TyApp ann fun arg)           = do
    vFun <- normalizeTypeM fun
    vArg <- normalizeTypeM arg
    case unNormalized vFun of
        TyLam _ nArg _ body -> do
            countTypeNormalizationStep
            substNormalizeTypeM vArg nArg body
        _                   -> pure $ TyApp ann <$> vFun <*> vArg
normalizeTypeM var@(TyVar _ name)            = do
    mayTy <- lookupTyNameM name
    case mayTy of
        Nothing -> pure $ Normalized var
        Just ty -> liftDupable ty
normalizeTypeM con@TyConstant{}              =
    pure $ Normalized con

{- Note [Normalizing substitution]
@substituteNormalize[M]@ is only ever used as normalizing substitution that receives two already
normalized types. However we do not enforce this in the type signature, because
1) it's perfectly correct for the last argument to be non-normalized
2) it would be annoying to wrap 'Type's into 'NormalizedType's
-}

-- See Note [Normalizing substitution].
-- | Substitute a type for a variable in a type and normalize in the 'NormalizeTypeM' monad.
substNormalizeTypeM
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => Normalized (Type tyname uni ann)  -- ^ @ty@
    -> tyname ann                        -- ^ @name@
    -> Type tyname uni ann               -- ^ @body@
    -> NormalizeTypeT m tyname uni ann (Normalized (Type tyname uni ann))
       -- ^ @NORM ([ty / name] body)@
substNormalizeTypeM ty name = withExtendedTypeVarEnv name ty . normalizeTypeM

-- | Normalize every 'Type' in a 'Term'.
normalizeTypesInM
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => Term tyname name uni ann -> NormalizeTypeT m tyname uni ann (Term tyname name uni ann)
normalizeTypesInM = transformMOf termSubterms normalizeChildTypes where
    normalizeChildTypes = termSubtypes (fmap unNormalized . normalizeTypeM)
