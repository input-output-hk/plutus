{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Renaming of PIR terms. Import this module to bring the @PLC.Rename (Term tyname name uni fun ann)@
-- instance in scope.
module PlutusIR.Transform.Rename () where

import           PlutusPrelude

import           PlutusIR
import           PlutusIR.Mark

import qualified PlutusCore                  as PLC
import qualified PlutusCore.Name             as PLC
import qualified PlutusCore.Rename.Internal  as PLC

import           Control.Monad.Reader
import           Control.Monad.Trans.Cont    (ContT (..))

import           Control.Monad.Except
import           Data.Set                    as Set
import qualified PlutusCore.Quote            as PLC
import           PlutusCore.StdLib.Data.List

{- Note [Renaming of mutually recursive bindings]
The 'RenameM' monad is a newtype wrapper around @ReaderT renaming Quote@, so in order to bring
a name into the scope we need to use the following pattern:

    withFreshenedX :: X -> (X -> RenameM renaming c) -> RenameM renaming c

where an 'X' is some value that has names inside and the second argument is a continuation that
expects to receive this value, but with all the names freshened and with the mappings from the
old uniques to the new ones saved. For the latter reason we can't just write

    freshenX :: X -> RenameM renaming X

because if you write

    freshenX x >>= \xFr -> ...

then the mappings from the uniques of 'x' to the uniques of 'xFr' do not exist in @...@.

Two problems arise:

1. If you have a list of @X@s and then an @Y@ and then another list of @Z@s and you need to freshen
   all of those, it's not nice to compose @withFreshened*@ functions at all. In order to make these
   functions composable, we can wrap them in the 'ContT' monad. See e.g. 'renameDatatypeCM' below.
2. Handling of mutually recursive bindings requires to bring a bunch of bindings in scope first and
   only then to rename their RHSs. I.e. one part of a 'ScopedRenameM' computation must be performed
   immediately (brinding names in scope) and the other part must be postponed until all bindings
   are in scope (renaming of RHSs). We will refer to this scheme as "two-stage renaming".

   Hence we have the following pattern:

       renameXCM :: X -> ContT c PLC.ScopedRenameM (PLC.ScopedRenameM X)

   where an 'X' is some value that defines some names and has types/terms that need to be renamed.
   The first 'PLC.ScopedRenameM' is for bringing names (the first stage) in scope and the second
   'PLC.ScopedRenameM' is for performing the renaming (the second stage).
-}

instance PLC.HasUniques (Term tyname name uni fun ann) => PLC.Rename (Term tyname name uni fun ann) where
    -- See Note [Marking]
    rename = through markNonFreshTerm >=> PLC.runRenameT . renameTermM

-- (let
--   (nonrec)
--   (datatypebind
--     (datatype
--       (tyvardecl M (fun (type) (type)))
--       (tyvardecl M (type))
--       match_M
--       (vardecl N [M M]) (vardecl J (fun M [M M]))
--     )
--   )
--   (abs a (type) (lam x a x))
-- )

newtype Restorer ren m = Restorer
    { unRestorer :: forall a. PLC.RenameT ren m a -> PLC.RenameT ren m a
    }

captureContext :: Monad m => (Restorer ren m -> PLC.RenameT ren m b) -> PLC.RenameT ren m b
captureContext k = do
    env <- ask
    k $ Restorer $ local $ const env

-- visibility (Datatype x dataDecl params matchName constrs) rest =
--   dataDecl `visibleIn` constrs
--   dataDecl `visibleIn` rest

-- params `visibleIn` constrs
-- params `notVisibleIn` rest

-- nonrec: List_0 `notVisibleIn` constrs => List_0 `staysIn` constrs
-- rec:    List_0 `visibleIn` constrs    => List_0 `notStaysIn` constrs

-- data Scope = Scope
--    { inScope    :: Set Name
--    , notInScope :: Set Name
--    }

-- defaultConstrs dataName params = [name1 :: iterTyApp dataName params, name2 :: iterTyApp dataName params]

-- notVisibleInConstrs ::

-- nonrec data List_2 a_1 where
--     Nil  :: List_2 a_1
--     Cons :: a_1 -> List_0 a_1 -> List_2 a_1

-- rec data List_2 a_1 where
--     Nil  :: List_2 a_1
--     Cons :: a_1 -> List_2 a_1 -> List_2 a_1

renameConstrTypeM
    :: (PLC.HasRenaming ren PLC.TypeUnique, PLC.HasUniques (Type tyname uni ann), PLC.MonadQuote m)
    => Restorer ren m -> Type tyname uni ann -> PLC.RenameT ren m (Type tyname uni ann)
renameConstrTypeM (Restorer restoreAfterData) = renameTyFunM where
    renameTyFunM (TyFun ann dom cod) = TyFun ann <$> PLC.renameTypeM dom <*> renameTyFunM cod
    renameTyFunM ty                  = renameTyAppM ty

    renameTyAppM (TyApp ann fun arg) = TyApp ann <$> renameTyAppM fun <*> PLC.renameTypeM arg
    renameTyAppM (TyVar ann name)    = TyVar ann <$> restoreAfterData (PLC.renameNameM name)
    renameTyAppM _                   =
        error "Panic: a constructor returns something that is not an iterated application of a type variable"

withFreshenedConstr
    :: (PLC.HasUniques (Term tyname name uni fun ann), PLC.MonadQuote m)
    => VarDecl tyname name uni fun ann
    -> ((Restorer PLC.ScopedRenaming m -> PLC.ScopedRenameT m (VarDecl tyname name uni fun ann)) ->
            PLC.ScopedRenameT m c)
    -> PLC.ScopedRenameT m c
withFreshenedConstr (VarDecl ann name ty) cont =
    PLC.withFreshenedName name $ \nameFr ->
        cont $ \restorerAfterData ->
            VarDecl ann nameFr <$> renameConstrTypeM restorerAfterData ty

-- withFreshenedVarDecl (VarDecl ann name ty) cont =
--     withFreshenedName name $ \nameFr -> cont $ VarDecl ann nameFr <$> renameTypeM ty

-- data Maybe a where
--     Nothing :: Maybe a
--     Just    :: a -> Maybe a

-- data List a where
--     Nil  :: List a
--     Cons :: a -> List a -> List a

onNonRec :: Recursivity -> (a -> a) -> a -> a
onNonRec NonRec f x = f x
onNonRec Rec    _ x = x

-- | Rename a 'Datatype' in the CPS-transformed 'ScopedRenameM' monad.
renameDatatypeCM
    :: (PLC.HasUniques (Term tyname name uni fun ann), PLC.MonadQuote m)
    => Recursivity
    -> Datatype tyname name uni fun ann
    -> ContT c (PLC.ScopedRenameT m) (PLC.ScopedRenameT m (Datatype tyname name uni fun ann))
renameDatatypeCM recy (Datatype x dataDecl params matchName constrs) = do
    -- The first stage (the data type itself, its constructors and its matcher get renamed).
    -- Note that all of these are visible downstream.
    constrsRen         <- traverse (ContT . withFreshenedConstr) constrs
    matchNameFr        <- ContT $ PLC.withFreshenedName matchName
    restorerBeforeData <- ContT captureContext
    dataDeclFr         <- ContT $ PLC.withFreshenedTyVarDecl dataDecl
    restorerAfterData  <- ContT captureContext
    -- The second stage (the type parameters and types of constructors get renamed).
    -- Note that parameters are only visible in the types of constructors and not downstream.
    pure . onNonRec recy (unRestorer restorerBeforeData) $
        runContT (traverse (ContT . PLC.withFreshenedTyVarDecl) params) $ \paramsFr ->
            Datatype x dataDeclFr paramsFr matchNameFr <$>
                traverse ($ restorerAfterData) constrsRen

-- | Rename a 'Binding' in the CPS-transformed 'ScopedRenameM' monad.
renameBindingNonRecC
    :: (PLC.HasUniques (Term tyname name uni fun ann), PLC.MonadQuote m)
    => Binding tyname name uni fun ann
    -> ContT c (PLC.ScopedRenameT m) (Binding tyname name uni fun ann)
renameBindingNonRecC = \case
    TermBind x s var term -> do
        termFr <- lift $ renameTermM term
        varFr <- lift =<< ContT (PLC.withFreshenedVarDecl var)
        pure $ TermBind x s varFr termFr
    TypeBind x var ty -> do
        tyFr <- lift $ PLC.renameTypeM ty
        varFr <- ContT $ PLC.withFreshenedTyVarDecl var
        pure $ TypeBind x varFr tyFr
    DatatypeBind x datatype ->
        fmap (DatatypeBind x) $ lift =<< renameDatatypeCM NonRec datatype

-- | Rename a 'Binding' in the CPS-transformed 'ScopedRenameM' monad.
renameBindingRecCM
    :: (PLC.HasUniques (Term tyname name uni fun ann), PLC.MonadQuote m)
    => Binding tyname name uni fun ann
    -> ContT c (PLC.ScopedRenameT m) (PLC.ScopedRenameT m (Binding tyname name uni fun ann))
renameBindingRecCM = \case
    TermBind x s var term -> do
        -- The first stage (the variable gets renamed).
        varRen <- ContT $ PLC.withFreshenedVarDecl var
        -- The second stage (the type of the variable and the RHS get renamed).
        pure $ TermBind x s <$> varRen <*> renameTermM term
    TypeBind x var ty -> do
        -- The first stage (the variable gets renamed).
        varFr <- ContT $ PLC.withFreshenedTyVarDecl var
        -- The second stage (the RHS gets renamed).
        pure $ TypeBind x varFr <$> PLC.renameTypeM ty
    DatatypeBind x datatype ->
        -- Both the stages of 'renameDatatypeCM'.
        fmap (DatatypeBind x) <$> renameDatatypeCM Rec datatype

-- | Replace the uniques in the names stored in a bunch of bindings by new uniques,
-- save the mapping from the old uniques to the new ones, rename the RHSs and
-- supply the updated bindings to a continuation.
withFreshenedBindings
    :: (PLC.HasUniques (Term tyname name uni fun ann), PLC.MonadQuote m)
    => Recursivity
    -> NonEmpty (Binding tyname name uni fun ann)
    -> (NonEmpty (Binding tyname name uni fun ann) -> PLC.ScopedRenameT m c)
    -> PLC.ScopedRenameT m c
withFreshenedBindings recy binds cont = case recy of
    -- Bring each binding in scope, rename its RHS straight away, collect all the results and
    -- supply them to the continuation.
    NonRec -> runContT (traverse renameBindingNonRecC binds) cont
    -- First bring all bindinds in scope and only then rename their RHSs and
    -- supply the results to the continuation.
    Rec    -> runContT (traverse renameBindingRecCM binds) $ sequence >=> cont

-- | Rename a 'Term' in the 'ScopedRenameM' monad.
renameTermM
    :: (PLC.HasUniques (Term tyname name uni fun ann), PLC.MonadQuote m)
    => Term tyname name uni fun ann -> PLC.ScopedRenameT m (Term tyname name uni fun ann)
renameTermM = \case
    Let x r binds term ->
        withFreshenedBindings r binds $ \bindsFr ->
            Let x r bindsFr <$> renameTermM term
    Var x name ->
        Var x <$> PLC.renameNameM name
    TyAbs x name kind body ->
        PLC.withFreshenedName name $ \nameFr ->
            TyAbs x nameFr kind <$> renameTermM body
    LamAbs x name ty body ->
        PLC.withFreshenedName name $ \nameFr ->
            LamAbs x nameFr <$> PLC.renameTypeM ty <*> renameTermM body
    Apply x fun arg ->
        Apply x <$> renameTermM fun <*> renameTermM arg
    Constant x con ->
        pure $ Constant x con
    Builtin x bi ->
        pure $ Builtin x bi
    TyInst x term ty ->
        TyInst x <$> renameTermM term <*> PLC.renameTypeM ty
    Error x ty ->
        Error x <$> PLC.renameTypeM ty
    IWrap x pat arg term ->
        IWrap x <$> PLC.renameTypeM pat <*> PLC.renameTypeM arg <*> renameTermM term
    Unwrap x term ->
        Unwrap x <$> renameTermM term
