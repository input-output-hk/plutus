{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE OverloadedStrings     #-}

module PlutusCore.Generators.Scoping where

import           PlutusCore
import           PlutusCore.Quote

import           Control.Monad.Except
import           Data.Coerce
import           Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Set             as Set

-- let nonrec x_1 = x_2 in x_3

-- let nonrec @(disappears_binding x_1) x_1 = @(stays_out_of_scope x_1) x_1 -> @(stays_free x_2) x_2 in @(disappears_bound x_1) x_1 -> @(stays_free x_3) x_3

-- let nonrec @(disappears_binding x_1) x_4 = @(stays_out_of_scope x_1) x_1 -> @(stays_free x_2) x_2 in @(disappears_bound x_1) x_4 -> @(stays_free x_3) x_3

-- @(disappears_binding x_1) x_4: x_1 must not be equal to x_4, x_1 is added to out_of_scope, x_4 is added to in_scope
-- @(stays_out_of_scope x_1) x_1: x_1 must be equal to x_1, x_1 must be in out_of_scope, x_1 must not be in in_scope
-- @(stays_free x_2) x_2: x_2 must be equal to x_2, x_2 must not be in out_of_scope, x_2 must not be in in_scope
-- @(disappears_bound x_1) x_4: x_1 must not be equal to x_4, x_1 must be in out_of_scope, x_4 must be in in_scope
-- @(stays_free x_3) x_3: x_3 must be equal to x_3, x_3 must not be in out_of_scope, x_3 must not be in in_scope



-- let nonrec x_1 = x_2 in x_3
-- let nonrec x_1 = x_2 in let nonrec x_1 = x_2 in x_3  -- Duplicating a binding makes the duplicate disappear


-- \x_1 -> x_2
-- x_1 (\x_1 -> x_1 x_2)
-- @(stays_out_of_scope x_1) x_1 (\(@disappears_binding x_1) x_1 -> @(disappears_variable x_1) x_1 @(stays_free x_2) x_2)
-- @(stays_out_of_scope x_1) x_1 (\(@disappears_binding x_1) x_3 -> @(disappears_variable x_1) x_3 @(stays_free x_2) x_2)

data ScopedName
    = TypeName TyName
    | TermName Name
    deriving (Show, Eq, Ord)

isSameScope :: ScopedName -> ScopedName -> Bool
isSameScope TypeName{} TypeName{} = True
isSameScope TermName{} TermName{} = True
isSameScope TypeName{} TermName{} = False
isSameScope TermName{} TypeName{} = False

data Persists
    = StaysOutOfScopeVariable
    | StaysFreeVariable
    deriving (Show)

data Changes
    = DisappearsBinding
    | DisappearsVariable
    deriving (Show)

data NameAction
    = Persists Persists
    | Changes Changes
    deriving (Show)

data NameAnn
    = NameAction NameAction ScopedName
    | NotAName
    deriving (Show)

class ToScopedName name where
    toScopedName :: name -> ScopedName

instance ToScopedName TyName where
    toScopedName = TypeName

instance ToScopedName Name where
    toScopedName = TermName

introduceBound :: ToScopedName name => name -> NameAnn
introduceBound = NameAction (Changes DisappearsBinding) . toScopedName

registerBound :: ToScopedName name => name -> NameAnn
registerBound = NameAction (Changes DisappearsVariable) . toScopedName

registerOutOfScope :: ToScopedName name => name -> NameAnn
registerOutOfScope = NameAction (Persists StaysOutOfScopeVariable) . toScopedName

registerFree :: ToScopedName name => name -> NameAnn
registerFree = NameAction (Persists StaysFreeVariable) . toScopedName

class Reference n t where
    referenceVia
        :: (forall name. ToScopedName name => name -> NameAnn)
        -> n
        -> t NameAnn
        -> t NameAnn

referenceInScope :: Reference n t => n -> t NameAnn -> t NameAnn
referenceInScope = referenceVia registerBound

referenceOutOfScope :: Reference n t => n -> t NameAnn -> t NameAnn
referenceOutOfScope = referenceVia registerOutOfScope

instance tyname ~ TyName => Reference TyName (Type tyname uni) where
    referenceVia reg tyname ty = TyApp NotAName ty $ TyVar (reg tyname) tyname

instance tyname ~ TyName => Reference TyName (Term tyname name uni fun) where
    referenceVia reg tyname term = TyInst NotAName term $ TyVar (reg tyname) tyname

instance name ~ Name => Reference Name (Term tyname name uni fun) where
    referenceVia reg name term = Apply NotAName term $ Var (reg name) name

-- #####################################################
-- ## Information about scopes and relevant functions ##
-- #####################################################

data ScopeEntry
    = DisappearedBindings
    | DisappearedVariables
    | AppearedBindings
    | AppearedVariables
    | StayedOutOfScopeVariables
    | StayedFreeVariables
    deriving (Show, Eq, Ord)

newtype ScopeInfo = ScopeInfo
    { unScopeInfo :: Map ScopeEntry (Set ScopedName)
    } deriving (Show)

to :: ScopeEntry -> ScopeInfo -> Set ScopedName
to entry = fromMaybe Set.empty . Map.lookup entry . unScopeInfo

emptyScopeInfo :: ScopeInfo
emptyScopeInfo = ScopeInfo Map.empty

mergeScopeInfo :: ScopeInfo -> ScopeInfo -> Either ScopeError ScopeInfo
mergeScopeInfo si1 si2
    | duplicateDisappearedBindings /= Set.empty =
        Left $ DuplicateBindersInTheInput duplicateDisappearedBindings
    | duplicateAppearedBindings /= Set.empty =
        Left $ DuplicateBindersInTheOutput duplicateAppearedBindings
    | otherwise = Right $ coerce (Map.unionWith Set.union) si1 si2
    where
        disappearedBindings1 = to DisappearedBindings si1
        disappearedBindings2 = to DisappearedBindings si2
        duplicateDisappearedBindings = disappearedBindings1 `Set.intersection` disappearedBindings2
        appearedBindings1 = to AppearedBindings si1
        appearedBindings2 = to AppearedBindings si2
        duplicateAppearedBindings = appearedBindings1 `Set.intersection` appearedBindings2

mergeErrOrScopeInfos :: [Either ScopeError ScopeInfo] -> Either ScopeError ScopeInfo
mergeErrOrScopeInfos =
    Prelude.foldr (\si1 si2 -> join $ mergeScopeInfo <$> si1 <*> si2) $ pure emptyScopeInfo

-- ########################################################################
-- ## Main class for collecting scope information and relevant functions ##
-- ########################################################################

class Scoping t where
    establishScoping :: MonadQuote m => t ann -> m (t NameAnn)

    -- We might want to use @Validation@ or something instead of 'Either'.
    collectScopeInfo :: t NameAnn -> Either ScopeError ScopeInfo

establishScopingBinder
    :: (Reference name lower, ToScopedName name, Scoping upper, Scoping lower, MonadQuote m)
    => (NameAnn -> name -> upper NameAnn -> lower NameAnn -> lower NameAnn)
    -> name
    -> upper ann
    -> lower ann
    -> m (lower NameAnn)
establishScopingBinder binder name upper lower = do
    upperS <- establishScoping upper
    referenceOutOfScope name .
        binder (introduceBound name) name upperS .
            referenceInScope name <$>
                establishScoping lower

-- #############################################
-- ## Checking coherence of scope information ##
-- #############################################

data ScopeError
    = UnannotatedName ScopedName
    | NameChangedItsScope ScopedName ScopedName
    | NameUnexpectedlyChanged ScopedName ScopedName
    | NameUnexpectedlyStayed ScopedName
    | DuplicateBindersInTheInput (Set ScopedName)
    | DuplicateBindersInTheOutput (Set ScopedName)
    | OldBindingsDiscordWithBoundVariables (Set ScopedName)
    | OldBindingsDiscordWithOutOfScopeVariables (Set ScopedName)
    | NewBindingsDiscordWithBoundVariables (Set ScopedName)
    | OldBindingsClashWithFreeVariables (Set ScopedName)
    | OldBindingsClashWithNewBindings (Set ScopedName)
    | NewBindingsClashWithFreeVariabes (Set ScopedName)
    deriving (Show)

overrideSname :: ScopeEntry -> ScopedName -> ScopeInfo -> ScopeInfo
overrideSname key = coerce . Map.insert key . Set.singleton

applyPersists :: Persists -> ScopedName -> ScopeInfo
applyPersists persists sname = overrideSname key sname emptyScopeInfo where
    key = case persists of
        StaysOutOfScopeVariable -> StayedOutOfScopeVariables
        StaysFreeVariable       -> StayedFreeVariables

applyChanges :: Changes -> ScopedName -> ScopedName -> ScopeInfo
applyChanges changes snameOld snameNew =
    overrideSname keyNew snameNew $ overrideSname keyOld snameOld emptyScopeInfo where
        (keyOld, keyNew) = case changes of
            DisappearsBinding  -> (DisappearedBindings, AppearedBindings)
            DisappearsVariable -> (DisappearedVariables, AppearedVariables)

applyNameAction
    :: NameAction -> ScopedName -> ScopedName -> Either ScopeError ScopeInfo
applyNameAction (Persists persists) snameOld snameNew =
    if snameOld == snameNew
        then Right $ applyPersists persists snameOld
        else Left $ NameUnexpectedlyChanged snameOld snameNew
applyNameAction (Changes changes) snameOld snameNew =
    if snameOld == snameNew
        then Left $ NameUnexpectedlyStayed snameOld
        else Right $ applyChanges changes snameOld snameNew

handleSname :: ToScopedName name => NameAnn -> name -> Either ScopeError ScopeInfo
handleSname ann nameNew = do
    let snameNew = toScopedName nameNew
    case ann of
        NotAName -> Left $ UnannotatedName snameNew
        NameAction action snameOld ->
            if snameOld `isSameScope` snameNew
                then applyNameAction action snameOld snameNew
                else Left $ NameChangedItsScope snameOld snameNew

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference s t = (s `Set.union` t) `Set.difference` (s `Set.intersection` t)

leftUnlessEmpty :: (Set ScopedName -> ScopeError) -> Set ScopedName -> Either ScopeError ()
leftUnlessEmpty err s = unless (Set.null s) . Left $ err s

checkScopeInfo :: ScopeInfo -> Either ScopeError ()
checkScopeInfo scopeInfo = do
    let disappearedBindings       = to DisappearedBindings       scopeInfo
        disappearedVariables      = to DisappearedVariables      scopeInfo
        appearedBindings          = to AppearedBindings          scopeInfo
        appearedVariables         = to AppearedVariables         scopeInfo
        stayedOutOfScopeVariables = to StayedOutOfScopeVariables scopeInfo
        stayedFreeVariables       = to StayedFreeVariables       scopeInfo
    -- The next three are based on the assumption that for each binder we add at least one
    -- out-of-scope variable and at least one in-scope one.
    leftUnlessEmpty OldBindingsDiscordWithBoundVariables $
        disappearedBindings `symmetricDifference` disappearedVariables
    leftUnlessEmpty OldBindingsDiscordWithOutOfScopeVariables $
        disappearedBindings `symmetricDifference` stayedOutOfScopeVariables
    leftUnlessEmpty NewBindingsDiscordWithBoundVariables $
        appearedBindings `symmetricDifference` appearedVariables
    leftUnlessEmpty OldBindingsClashWithFreeVariables $
        disappearedBindings `Set.intersection` stayedFreeVariables
    leftUnlessEmpty OldBindingsClashWithNewBindings $
        disappearedBindings `Set.intersection` appearedBindings
    leftUnlessEmpty NewBindingsClashWithFreeVariabes $
        appearedBindings `Set.intersection` stayedFreeVariables

checkRespectsScoping :: Scoping t => (t NameAnn -> t NameAnn) -> t ann -> Either ScopeError ()
checkRespectsScoping ren =
    checkScopeInfo <=< collectScopeInfo . ren . runQuote . establishScoping





instance Scoping Kind where
    establishScoping kind = pure $ NotAName <$ kind
    collectScopeInfo _ = Right emptyScopeInfo

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
        mergeErrOrScopeInfos [handleSname ann name, collectScopeInfo kind, collectScopeInfo ty]
    collectScopeInfo (TyForall ann name kind ty) =
        mergeErrOrScopeInfos [handleSname ann name, collectScopeInfo kind, collectScopeInfo ty]
    collectScopeInfo (TyIFix _ pat arg) =
        mergeErrOrScopeInfos [collectScopeInfo pat, collectScopeInfo arg]
    collectScopeInfo (TyApp _ fun arg) =
        mergeErrOrScopeInfos [collectScopeInfo fun, collectScopeInfo arg]
    collectScopeInfo (TyFun _ dom cod) =
        mergeErrOrScopeInfos [collectScopeInfo dom, collectScopeInfo cod]
    collectScopeInfo (TyVar ann name) = handleSname ann name
    collectScopeInfo (TyBuiltin _ _) = Right emptyScopeInfo

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

    collectScopeInfo (LamAbs ann name ty body)  = do
        mergeErrOrScopeInfos [handleSname ann name, collectScopeInfo ty, collectScopeInfo body]
    collectScopeInfo (TyAbs ann name kind body) = do
        mergeErrOrScopeInfos [handleSname ann name, collectScopeInfo kind, collectScopeInfo body]
    collectScopeInfo (IWrap _ pat arg term)   =
        mergeErrOrScopeInfos [collectScopeInfo pat, collectScopeInfo arg, collectScopeInfo term]
    collectScopeInfo (Apply _ fun arg) =
        mergeErrOrScopeInfos [collectScopeInfo fun, collectScopeInfo arg]
    collectScopeInfo (Unwrap _ term) = collectScopeInfo term
    collectScopeInfo (Error _ ty) = collectScopeInfo ty
    collectScopeInfo (TyInst _ term ty) =
        mergeErrOrScopeInfos [collectScopeInfo term, collectScopeInfo ty]
    collectScopeInfo (Var ann name) = handleSname ann name
    collectScopeInfo (Constant _ _) = Right emptyScopeInfo
    collectScopeInfo (Builtin _ _) = Right emptyScopeInfo

instance (tyname ~ TyName, name ~ Name) => Scoping (Program tyname name uni fun) where
    establishScoping (Program _ ver term) =
        Program NotAName (NotAName <$ ver) <$> establishScoping term

    collectScopeInfo (Program _ _ term) = collectScopeInfo term

-- flawed renamers:
--   changing free variables
--   not changing bound variables correctly
--   changing the spine of the program
--   mixing up type and term variables
--
--   changing names at the binding site but not the use site (or the other way around)
--   changing out-of-scope variables
--   adding names clashing with preexisting names

--   leaving duplicate binders



    -- leftUnlessEmpty OldBindingsDiscordWithBoundVariables $
    --     disappearedBindings `symmetricDifference` disappearedVariables
    -- leftUnlessEmpty OldBindingsDiscordWithOutOfScopeVariables $
    --     disappearedBindings `symmetricDifference` stayedOutOfScopeVariables
    -- leftUnlessEmpty NewBindingsDiscordWithBoundVariables $
    --     appearedBindings `symmetricDifference` appearedVariables
    -- leftUnlessEmpty OldBindingsClashWithFreeVariables $
    --     disappearedBindings `Set.intersection` stayedFreeVariables
    -- leftUnlessEmpty OldBindingsClashWithNewBindings $
    --     disappearedBindings `Set.intersection` appearedBindings
    -- leftUnlessEmpty NewBindingsClashWithFreeVariabes $
    --     appearedBindings `Set.intersection` stayedFreeVariables
