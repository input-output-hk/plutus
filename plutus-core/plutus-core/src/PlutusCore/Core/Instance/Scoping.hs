{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusCore.Core.Instance.Scoping () where

import           PlutusCore.Check.Scoping
import           PlutusCore.Core.Type
import           PlutusCore.Name
import           PlutusCore.Quote

-- In the three instances below the added variable is always the last field of a constructor.
-- Just to be consistent.

instance tyname ~ TyName => Reference TyName (Type tyname uni) where
    referenceVia reg tyname ty = TyApp NotAName ty $ TyVar (reg tyname) tyname

instance tyname ~ TyName => Reference TyName (Term tyname name uni fun) where
    referenceVia reg tyname term = TyInst NotAName term $ TyVar (reg tyname) tyname

instance name ~ Name => Reference Name (Term tyname name uni fun) where
    referenceVia reg name term = Apply NotAName term $ Var (reg name) name

-- Kinds have no names, hence the simple instance.
instance Scoping Kind where
    establishScoping kind = pure $ NotAName <$ kind
    collectScopeInfo _ = mempty

instance tyname ~ TyName => Scoping (Type tyname uni) where
    establishScoping (TyLam _ nameDup kind ty) = do
        name <- freshenTyName nameDup
        establishScopingBinder TyLam name kind ty
    establishScoping (TyForall _ nameDup kind ty) = do
        name <- freshenTyName nameDup
        establishScopingBinder TyForall name kind ty
    establishScoping (TyIFix _ pat arg) =
        TyIFix NotAName <$> establishScoping pat <*> establishScoping arg
    establishScoping (TyApp _ fun arg) =
        TyApp NotAName <$> establishScoping fun <*> establishScoping arg
    establishScoping (TyFun _ dom cod) =
        TyFun NotAName <$> establishScoping dom <*> establishScoping cod
    establishScoping (TyVar _ nameDup) = do
        name <- freshenTyName nameDup
        pure $ TyVar (registerFree name) name
    establishScoping (TyBuiltin _ fun) = pure $ TyBuiltin NotAName fun

    collectScopeInfo (TyLam ann name kind ty) =
        handleSname ann name <> collectScopeInfo kind <> collectScopeInfo ty
    collectScopeInfo (TyForall ann name kind ty) =
        handleSname ann name <> collectScopeInfo kind <> collectScopeInfo ty
    collectScopeInfo (TyIFix _ pat arg) =
        collectScopeInfo pat <> collectScopeInfo arg
    collectScopeInfo (TyApp _ fun arg) =
        collectScopeInfo fun <> collectScopeInfo arg
    collectScopeInfo (TyFun _ dom cod) =
        collectScopeInfo dom <> collectScopeInfo cod
    collectScopeInfo (TyVar ann name) = handleSname ann name
    collectScopeInfo (TyBuiltin _ _) = mempty

instance (tyname ~ TyName, name ~ Name) => Scoping (Term tyname name uni fun) where
    establishScoping (LamAbs _ nameDup ty body)  = do
        name <- freshenName nameDup
        establishScopingBinder LamAbs name ty body
    establishScoping (TyAbs _ nameDup kind body) = do
        name <- freshenTyName nameDup
        establishScopingBinder TyAbs name kind body
    establishScoping (IWrap _ pat arg term)   =
        IWrap NotAName <$> establishScoping pat <*> establishScoping arg <*> establishScoping term
    establishScoping (Apply _ fun arg) =
        Apply NotAName <$> establishScoping fun <*> establishScoping arg
    establishScoping (Unwrap _ term) = Unwrap NotAName <$> establishScoping term
    establishScoping (Error _ ty) = Error NotAName <$> establishScoping ty
    establishScoping (TyInst _ term ty) =
        TyInst NotAName <$> establishScoping term <*> establishScoping ty
    establishScoping (Var _ nameDup) = do
        name <- freshenName nameDup
        pure $ Var (registerFree name) name
    establishScoping (Constant _ con) = pure $ Constant NotAName con
    establishScoping (Builtin _ bi) = pure $ Builtin NotAName bi

    collectScopeInfo (LamAbs ann name ty body)  =
        handleSname ann name <> collectScopeInfo ty <> collectScopeInfo body
    collectScopeInfo (TyAbs ann name kind body) =
        handleSname ann name <> collectScopeInfo kind <> collectScopeInfo body
    collectScopeInfo (IWrap _ pat arg term)   =
        collectScopeInfo pat <> collectScopeInfo arg <> collectScopeInfo term
    collectScopeInfo (Apply _ fun arg) =
        collectScopeInfo fun <> collectScopeInfo arg
    collectScopeInfo (Unwrap _ term) = collectScopeInfo term
    collectScopeInfo (Error _ ty) = collectScopeInfo ty
    collectScopeInfo (TyInst _ term ty) =
        collectScopeInfo term <> collectScopeInfo ty
    collectScopeInfo (Var ann name) = handleSname ann name
    collectScopeInfo (Constant _ _) = mempty
    collectScopeInfo (Builtin _ _) = mempty

instance (tyname ~ TyName, name ~ Name) => Scoping (Program tyname name uni fun) where
    establishScoping (Program _ ver term) =
        Program NotAName (NotAName <$ ver) <$> establishScoping term

    collectScopeInfo (Program _ _ term) = collectScopeInfo term
