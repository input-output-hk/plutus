{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module PlutusIR.Core.Instance.Scoping where


import           PlutusIR.Core.Type

import           PlutusCore.Check.Scoping
import           PlutusCore.MkPlc
import           PlutusCore.Quote

import           Data.Foldable
import           Data.List.NonEmpty       (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty       as NonEmpty
import           Data.Traversable

-- Should we only pick an arbitrary sublist of the provided list instead of using the whole list?
instance Reference n t => Reference [n] t where
    referenceVia reg = flip . foldr $ referenceVia reg

instance Reference n t => Reference (NonEmpty n) t where
    referenceVia reg = referenceVia reg . NonEmpty.toList

instance tyname ~ TyName => Reference TyName (Term tyname name uni fun) where
    referenceVia reg tyname term = TyInst NotAName term $ TyVar (reg tyname) tyname

instance name ~ Name => Reference Name (Term tyname name uni fun) where
    referenceVia reg name term = Apply NotAName term $ Var (reg name) name

instance tyname ~ TyName => Reference TyName (VarDecl tyname name uni fun) where
    referenceVia reg tyname (VarDecl ann varName ty) =
        VarDecl ann varName $ referenceVia reg tyname ty

instance tyname ~ TyName => Reference TyName (Datatype tyname name uni fun) where
    referenceVia reg tyname (Datatype dataAnn dataDecl params matchName constrs) =
        Datatype dataAnn dataDecl params matchName $ map goConstr constrs where
            goConstr (VarDecl ann constrName constrTy) = VarDecl ann constrName $ goTyFun constrTy

            goTyFun (TyFun ann dom cod) = TyFun ann (referenceVia reg tyname dom) $ goTyFun cod
            goTyFun ty                  = goTyApp ty

            goTyApp (TyApp ann fun arg) = TyApp ann (goTyApp fun) $ referenceVia reg tyname arg
            goTyApp ty                  = ty

instance tyname ~ TyName => Reference TyName (Binding tyname name uni fun) where
    referenceVia reg tyname (TermBind ann strictness varDecl term) =
        TermBind ann strictness (referenceVia reg tyname varDecl) $ referenceVia reg tyname term
    referenceVia reg tyname (TypeBind ann tyVarDecl ty) =
        TypeBind ann tyVarDecl $ referenceVia reg tyname ty
    referenceVia reg tyname (DatatypeBind ann datatype) =
        DatatypeBind ann $ referenceVia reg tyname datatype

-- Note that unlike other 'Reference' instances this one does not guarantee that name will actually
-- be referenced.
instance name ~ Name => Reference Name (Binding tyname name uni fun) where
    referenceVia reg name (TermBind ann strictness varDecl term) =
        TermBind ann strictness varDecl $ referenceVia reg name term
    referenceVia _ _ typeBind@TypeBind{} = typeBind
    referenceVia _ _ datatypeBind@DatatypeBind{} = datatypeBind

instance Reference tyname t => Reference (TyVarDecl tyname ann) t where
    referenceVia reg = referenceVia reg . tyVarDeclName

instance Reference name t => Reference (VarDecl tyname name uni fun ann) t where
    referenceVia reg = referenceVia reg . varDeclName

instance (Reference TyName t, Reference Name t) => Reference (Datatype TyName Name uni fun ann) t where
    referenceVia reg (Datatype _ dataDecl params matchName constrs)
        = referenceVia reg dataDecl
          -- Parameters of a data type are not visible outside of the data type no matter what.
        . referenceOutOfScope params
        . referenceVia reg matchName
        . referenceVia reg constrs

instance (Reference TyName t, Reference Name t) => Reference (Binding TyName Name uni fun ann) t where
    referenceVia reg (TermBind _ _ varDecl _)  = referenceVia reg varDecl
    referenceVia reg (TypeBind _ tyVarDecl _)  = referenceVia reg tyVarDecl
    referenceVia reg (DatatypeBind _ datatype) = referenceVia reg datatype

establishScopingParamsNonRec
    :: MonadQuote m => [TyVarDecl TyName ann] -> m [TyVarDecl TyName NameAnn]
establishScopingParamsNonRec =
    traverse $ \(TyVarDecl _ paramNameDup paramKind) -> do
        paramName <- freshenTyName paramNameDup
        TyVarDecl (introduceBound paramName) paramName <$> establishScoping paramKind

dataParamsIterApp
    :: (TyName -> NameAnn)
    -> TyName
    -> [TyVarDecl TyName NameAnn]
    -> Type TyName uni NameAnn
dataParamsIterApp reg dataName
    = mkIterTyApp NotAName (TyVar (reg dataName) dataName)
    . map (\(TyVarDecl _ name _) -> TyVar (registerBound name) name)

establishScopingConstrTyNonRec
    :: MonadQuote m
    => TyName
    -> [TyVarDecl TyName NameAnn]
    -> Type TyName uni ann
    -> m (Type TyName uni NameAnn)
establishScopingConstrTyNonRec dataName params = goTyFun where
    goTyFun (TyFun _ dom cod) = TyFun NotAName <$> establishScoping dom <*> goTyFun cod
    goTyFun ty                =
        TyFun NotAName (dataParamsIterApp registerOutOfScope dataName params) <$> goTyApp ty

    goTyApp (TyApp _ fun arg) = TyApp NotAName <$> goTyApp fun <*> establishScoping arg
    goTyApp _                 = pure $ dataParamsIterApp registerBound dataName params

establishScopingConstrsNonRec
    :: MonadQuote m
    => TyName
    -> [TyVarDecl TyName NameAnn]
    -> [VarDecl TyName Name uni fun ann]
    -> m [VarDecl TyName Name uni fun NameAnn]
establishScopingConstrsNonRec dataName params constrs = do
    -- TODO: explain.
    cons0 <- freshName "cons0"
    let cons0Decl
            = VarDecl (introduceBound cons0) cons0
            . TyFun NotAName (dataParamsIterApp registerOutOfScope dataName params)
            $ dataParamsIterApp registerBound dataName params
    fmap (cons0Decl :) . for constrs $ \(VarDecl _ constrNameDup constrTyDup) -> do
        constrName <- freshenName constrNameDup
        constrTy <- establishScopingConstrTyNonRec dataName params constrTyDup
        pure $ VarDecl (introduceBound constrName) constrName constrTy

establishScopingBindingNonRec
    :: MonadQuote m => Binding TyName Name uni fun ann -> m (Binding TyName Name uni fun NameAnn)
establishScopingBindingNonRec (TermBind _ strictness (VarDecl _ nameDup ty) term) = do
    name <- freshenName nameDup
    varDecl <- VarDecl (introduceBound name) name <$> establishScoping ty
    TermBind NotAName strictness varDecl . referenceOutOfScope name <$> establishScoping term
establishScopingBindingNonRec (TypeBind _ (TyVarDecl _ nameDup kind) ty) = do
    name <- freshenTyName nameDup
    tyVarDecl <- TyVarDecl (introduceBound name) name <$> establishScoping kind
    TypeBind NotAName tyVarDecl . referenceOutOfScope name <$> establishScoping ty
establishScopingBindingNonRec (DatatypeBind _ datatypeDup) = do
    let Datatype _ (TyVarDecl _ dataNameDup dataKind) paramsDup matchNameDup constrsDup = datatypeDup
    dataName <- freshenTyName dataNameDup
    dataDecl <- TyVarDecl (introduceBound dataName) dataName <$> establishScoping dataKind
    params <- establishScopingParamsNonRec paramsDup
    matchName <- freshenName matchNameDup
    constrs <- establishScopingConstrsNonRec dataName params constrsDup
    let datatype = Datatype (introduceBound matchName) dataDecl params matchName constrs
    pure $ DatatypeBind NotAName datatype

referenceBindingsNonRec
    :: NonEmpty (Binding TyName Name uni fun NameAnn)
    -> NonEmpty (Binding TyName Name uni fun NameAnn)
referenceBindingsNonRec (b0 :| [])  = b0 :| []
referenceBindingsNonRec (b0 :| bs0) = go [] b0 bs0 where
    go prevs b []       = referenceInScope prevs b :| []
    go prevs b (c : bs) = b <| go (b : prevs) c bs

establishScopingBindingsNonRec
    :: MonadQuote m
    => NonEmpty (Binding TyName Name uni fun ann)
    -> m (NonEmpty (Binding TyName Name uni fun NameAnn))
establishScopingBindingsNonRec =
    fmap referenceBindingsNonRec . traverse establishScopingBindingNonRec

collectScopeInfoTyVarDecl :: TyVarDecl TyName NameAnn -> ScopeErrorOrInfo
collectScopeInfoTyVarDecl (TyVarDecl ann tyname kind) =
    handleSname ann tyname <> collectScopeInfo kind

collectScopeInfoVarDecl :: VarDecl TyName Name uni fun NameAnn -> ScopeErrorOrInfo
collectScopeInfoVarDecl (VarDecl ann name ty) =
    handleSname ann name <> collectScopeInfo ty

collectScopeInfoDatatype :: Datatype TyName Name uni fun NameAnn -> ScopeErrorOrInfo
collectScopeInfoDatatype (Datatype matchAnn dataDecl params matchName constrs) = fold
    [ collectScopeInfoTyVarDecl dataDecl
    , foldMap collectScopeInfoTyVarDecl params
    , handleSname matchAnn matchName
    , foldMap collectScopeInfoVarDecl constrs
    ]

-- TODO: use a type class for collecting.
collectScopeInfoBinding :: Binding TyName Name uni fun NameAnn -> ScopeErrorOrInfo
collectScopeInfoBinding (TermBind _ _ varDecl term) =
    collectScopeInfoVarDecl varDecl <> collectScopeInfo term
collectScopeInfoBinding (TypeBind _ tyVarDecl ty) =
    collectScopeInfoTyVarDecl tyVarDecl <> collectScopeInfo ty
collectScopeInfoBinding (DatatypeBind _ datatype) =
    collectScopeInfoDatatype datatype

-- DON'T FORGET TO HANDLE OUT OF SCOPE THINGS (IN PARTICULAR, PARAMS)
instance (tyname ~ TyName, name ~ Name) => Scoping (Term tyname name uni fun) where
    establishScoping (Let _ recy bindingsDup body) = do
        bindings <- establishScopingBindingsNonRec bindingsDup
        referenceOutOfScope bindings . Let NotAName recy bindings . referenceInScope bindings <$>
            establishScoping body
    establishScoping (LamAbs _ nameDup ty body) = do
        name <- freshenName nameDup
        establishScopingBinder LamAbs name ty body
    establishScoping (TyAbs _ nameDup kind body) = do
        name <- freshenTyName nameDup
        establishScopingBinder TyAbs name kind body
    establishScoping (IWrap _ pat arg term) =
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

    -- TODO: HANDLE
    collectScopeInfo (Let _ Rec _ _) = Prelude.error "can't handle right now"
    collectScopeInfo (Let _ NonRec bindings body) =
        foldMap collectScopeInfoBinding bindings <> collectScopeInfo body
    collectScopeInfo (LamAbs ann name ty body) =
        handleSname ann name <> collectScopeInfo ty <> collectScopeInfo body
    collectScopeInfo (TyAbs ann name kind body) =
        handleSname ann name <> collectScopeInfo kind <> collectScopeInfo body
    collectScopeInfo (IWrap _ pat arg term) =
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
    establishScoping (Program _ term) =
        Program NotAName <$> establishScoping term

    collectScopeInfo (Program _ term) = collectScopeInfo term
