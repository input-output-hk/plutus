{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveAnyClass, DataKinds, KindSignatures #-}
module StubTypes where

import Data.Data (Data)
import Data.String (IsString(..))
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Identity
import Data.ByteString
import qualified Control.Exception as Exception
import qualified Language.Haskell.TH as TH

data DynFlags    = DynFlags_
type FamInstEnvs = (FamInstEnv, FamInstEnv)
data Name        = Name_ deriving (Eq, Ord, Outputable, Data)
data OccName     = OccName_ deriving (Eq, Ord)
data Module      = Module_ deriving (Eq, Ord)
data UnitId      = UnitId_
data TyThing     = TyThing_
data ModSummary  = ModSummary_
data TcGblEnv    = TcGblEnv_
data LHsExpr a   = LHsExpr_
data GhcTc       = GhcTc_
data GhcRn       = GhcRn_
data GhcException
  = CmdLineError String
  | ProgramError String
  | PprProgramError String SDoc
  deriving (Show, Exception.Exception)
data ModuleName  = ModuleName_ deriving (Eq, Ord, Data, Outputable)
data SDoc        = SDoc_ deriving Show
data HsParsedModule = HsParsedModule_
data HsGroup a   = HsGroup_
data Phase       = Phase_
data Coercion  = Coercion_ deriving Data
data Type      = Type_ deriving (Data, Outputable)
type Kind      = Type
type TyVar = Var
data SrcSpan = SrcSpan_ deriving (Eq, Ord, Data, Outputable)
data RealSrcSpan = RealSrcSpan_ deriving (Data, Outputable)
data Tickish a =
    SourceNote { sourceSpan :: RealSrcSpan -- ^ Source covered
               , sourceName :: String      -- ^ Name for source location
    }
               deriving Data
data Var       = Var_ deriving (Eq, Data, Outputable)
type Id = Var
data Fingerprint = Fingerprint_ deriving Outputable
data PrintUnqualified = PrintUnqualified_
data TyCon = TyCon_ deriving (Eq, Outputable)
data IdDetails = ClassOpId Class
               | DataConWorkId DataCon
               | PrimOpId PrimOp
               deriving Outputable
data IdUnfolding = IdUnfolding_ deriving Outputable
data Unfolding = Unfolding_ deriving Outputable
data FunctionOrData = IsFunction | IsData
    deriving (Eq, Ord, Data, Outputable)
data FV = FV_
data Class = Class_
data NameSpace = NameSpace_
data HscEnv = HscEnv { hsc_dflags :: DynFlags }
data RdrName = RdrName_
data Messages = Messages_

data Literal
    =     ------------------
          -- First the primitive guys
      LitChar    Char            -- ^ @Char#@ - at least 31 bits. Create with 'mkMachChar'

    | LitNumber !LitNumType !Integer Type
        --  ^ Any numeric literal that can be
        -- internally represented with an Integer
    | LitString   ByteString      -- ^ A string-literal: stored and emitted
                                  -- UTF-8 encoded, we'll arrange to decode it
                                  -- at runtime.  Also emitted with a @'\0'@
                                  -- terminator. Create with 'mkMachString'

    | LitNullAddr                 -- ^ The @NULL@ pointer, the only pointer value
                                  -- that can be represented as a Literal. Create
                                  -- with 'nullAddrLit'

    | LitRubbish                  -- ^ A nonsense value, used when an unlifted
                                  -- binding is absent and has type
                                  -- @forall (a :: 'TYPE' 'UnliftedRep'). a@.
                                  -- May be lowered by code-gen to any possible
                                  -- value. Also see Note [Rubbish literals]

    | LitFloat   Rational         -- ^ @Float#@. Create with 'mkMachFloat'
    | LitDouble  Rational         -- ^ @Double#@. Create with 'mkMachDouble'

    | LitLabel   FastString
                  (Maybe Int)
          FunctionOrData
                  -- ^ A label literal. Parameters:
                  --
                  -- 1) The name of the symbol mentioned in the declaration
                  --
                  -- 2) The size (in bytes) of the arguments
                  --    the label expects. Only applicable with
                  --    @stdcall@ labels. @Just x@ => @\<x\>@ will
                  --    be appended to label name when emitting assembly.
    deriving Data

-- | Numeric literal type
data LitNumType
    = LitNumInteger -- ^ @Integer@ (see Note [Integer literals])
    | LitNumNatural -- ^ @Natural@ (see Note [Natural literals])
    | LitNumInt     -- ^ @Int#@ - according to target machine
    | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
    | LitNumWord    -- ^ @Word#@ - according to target machine
    | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
    deriving (Data,Enum,Eq,Ord)

type FastString = String

data AlgTyConRhs =
             AbstractTyCon
           | DataTyCon { data_cons :: [DataCon] }
           | TupleTyCon { data_con :: DataCon }
           | SumTyCon { data_cons :: [DataCon] }
           | NewTyCon { data_con :: DataCon }
data DataCon = DataCon_ deriving (Eq, Data, Outputable)
data Role = Representational
data CoAxiom (a :: BranchFlag) = CoAxiom_
data BranchFlag = BFBranched | BFUnbranched
type Unbranched = 'BFUnbranched

data PrimOp = IntAddOp
            | IntSubOp
            | IntMulOp
            | IntQuotOp
            | IntRemOp
            | IntGtOp
            | IntGeOp
            | IntLtOp
            | IntLeOp
            | IntEqOp
    deriving (Eq, Ord, Enum, Outputable)

data Expr b
    = Var   Id
    | Lit   Literal
    | App   (Expr b) (Arg b)
    | Lam   b (Expr b)
    | Let   (Bind b) (Expr b)
    | Case  (Expr b) b Type [Alt b]       -- See #case_invariants#
    | Cast  (Expr b) Coercion
    | Tick  (Tickish Id) (Expr b)
    | Type  Type
    | Coercion Coercion
    deriving (Data, Outputable)

data Bind b
    = NonRec b (Expr b)
    | Rec [(b, (Expr b))]
    deriving Data

data AltCon
    = DataAlt DataCon   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                        -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@

    | LitAlt  Literal   -- ^ A literal: @case e of { 1 -> ... }@
                        -- Invariant: always an *unlifted* literal
                        -- See Note [Literal alternatives]

    | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
        deriving (Data, Outputable)

type CoreExpr = Expr CoreBndr
type CoreBndr = Var
type CoreAlt  = Alt  CoreBndr
type CoreBind = Bind CoreBndr
type CoreProgram = [CoreBind]
type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data ImpDeclSpec
    = ImpDeclSpec {
          is_mod      :: ModuleName, -- ^ Module imported, e.g. @import Muggle@
                                     -- Note the @Muggle@ may well not be
                                     -- the defining module for this thing!

                                     -- TODO: either should be Module, or there
                                     -- should be a Maybe UnitId here too.
          is_as       :: ModuleName, -- ^ Import alias, e.g. from @as M@ (or @Muggle@ if there is no @as@ clause)
          is_qual     :: Bool,       -- ^ Was this import qualified?
          is_dloc     :: SrcSpan     -- ^ The location of the entire import declaration
      } deriving (Eq, Ord, Data)

data ImpItemSpec
    = ImpAll
    | ImpSome {
          is_explicit :: Bool,
          is_iloc     :: SrcSpan
    }
    deriving (Eq, Ord, Data)

data ImportSpec = ImpSpec { is_decl :: ImpDeclSpec,
                            is_item :: ImpItemSpec }
    deriving( Eq, Ord, Data )

data ModIface = ModIface { mi_exports :: [IfaceExport] }
data ModGuts = ModGuts { mg_fam_inst_env :: FamInstEnv }

data FamInstEnv = FamInstEnv_

data SimplMode             -- See comments in SimplMonad
  = SimplMode
        { sm_names      :: [String] -- Name(s) of the phase
        , sm_phase      :: CompilerPhase
        , sm_dflags     :: DynFlags -- Just for convenient non-monadic
                                    -- access; we don't override these
        , sm_rules      :: Bool     -- Whether RULES are enabled
        , sm_inline     :: Bool     -- Whether inlining is enabled
        , sm_case_case  :: Bool     -- Whether case-of-case is enabled
        , sm_eta_expand :: Bool     -- Whether eta-expansion is enabled
        } deriving Outputable

data CompilerPhase
    = Phase PhaseNum
    | InitialPhase    -- The first phase -- number = infinity!

type PhaseNum = Int

data CoreToDo           -- These are diff core-to-core passes,
                        -- which may be invoked in any order,
                        -- as many times as you like.

  = CoreDoSimplify      -- The core-to-core simplifier.
        Int                    -- Max iterations
        SimplMode
  | CoreDoPluginPass String CorePluginPass
  | CoreDoFloatInwards
  | CoreDoFloatOutwards FloatOutSwitches
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoCallArity
  | CoreDoExitify
  | CoreDoStrictness
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoSpecConstr
  | CoreCSE
  | CoreDoRuleCheck CompilerPhase String   -- Check for non-application of rules
                                           -- matching this string
  | CoreDoNothing                -- Useful when building up
  | CoreDoPasses [CoreToDo]      -- lists of these things

  | CoreDesugar    -- Right after desugaring, no simple optimisation yet!
  | CoreDesugarOpt -- CoreDesugarXXX: Not strictly a core-to-core pass, but produces
                       --                 Core output, and hence useful to pass to endPass

  | CoreTidy
  | CorePrep
  | CoreOccurAnal

data GlobalRdrElt
  = GRE { gre_name :: Name
        , gre_par  :: Parent
        , gre_lcl :: Bool          -- ^ True <=> the thing was defined locally
        , gre_imp :: [ImportSpec]  -- ^ In scope through these imports
    } deriving (Data, Eq)

data Parent = Parent_ deriving (Eq, Data)

data FloatOutSwitches = FloatOutSwitches_

type CorePluginPass = ModGuts -> CoreM ModGuts

type IfaceExport = AvailInfo
data AvailInfo = AvailInfo_
data GlobalRdrEnv = GlobalRdrEnv_

data UniqSet a = UniqSet_
instance Semigroup (UniqSet a) where _ <> _ = UniqSet_
instance Monoid (UniqSet a) where mempty = UniqSet_

data StubM a = StubM_
instance Functor StubM where fmap _ _ = StubM_
instance Applicative StubM where pure _ = StubM_
instance Monad StubM

newtype Hsc a       = Hsc_ (StubM a) deriving (Functor, Applicative, Monad)
newtype CoreM a     = CoreM_ (StubM a) deriving (Functor, Applicative, Monad)
instance MonadIO CoreM where
    liftIO = undefined
-- type CoreM a = IO a
newtype TcM a       = TcM_ (StubM a) deriving (Functor, Applicative, Monad)
type TcRn a = TcM a
newtype IfM ab a    = IfM_ (StubM a) deriving (Functor, Applicative, Monad)
newtype Ghc a       = Ghc_ (StubM a) deriving (Functor, Applicative, Monad)

data ModLocation = ModLocation_
data ModuleOrigin = ModuleOrigin_
data UnusablePackageReason = UnusablePackageReason_
data ModuleSuggestion = ModuleSuggestion_

data FindResult
    = Found ModLocation Module
          -- ^ The module was found
    | NoPackage UnitId
          -- ^ The requested package was not found
    | FoundMultiple [(Module, ModuleOrigin)]
          -- ^ _Error_: both in multiple packages

          -- | Not found
    | NotFound
        { fr_paths       :: [FilePath]       -- Places where I looked

        , fr_pkg         :: Maybe UnitId  -- Just p => module is in this package's
                                             --           manifest, but couldn't find
                                             --           the .hi file

        , fr_mods_hidden :: [UnitId]      -- Module is in these packages,
                                             --   but the *module* is hidden

        , fr_pkgs_hidden :: [UnitId]      -- Module is in these packages,
                                             --   but the *package* is hidden

          -- Modules are in these packages, but it is unusable
        , fr_unusables   :: [(UnitId, UnusablePackageReason)]

        , fr_suggestions :: [ModuleSuggestion] -- Possible mis-spelled modules
        }

class Outputable a where
    ppr :: a -> SDoc
    pprPrec :: Rational -> a -> SDoc
    ppr = pprPrec 0
    pprPrec _  _ = SDoc_

occName :: Name -> OccName
occName _ = OccName_

occNameString :: OccName -> String
occNameString _ = "OccName"

moduleName :: Module -> ModuleName
moduleName _ = ModuleName_

nameModule_maybe :: Name -> Maybe Module
nameModule_maybe _ = Just Module_

moduleUnitId :: Module -> UnitId
moduleUnitId _ = UnitId_

stableModuleNameCmp :: ModuleName -> ModuleName -> Ordering
stableModuleNameCmp _ _ = EQ

stableUnitIdCmp :: UnitId -> UnitId -> Ordering
stableUnitIdCmp _ _ = EQ

fingerprintString :: String -> Fingerprint
fingerprintString _ = Fingerprint_

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints _ = Fingerprint_

(<+>) :: SDoc -> SDoc -> SDoc
(<+>) _ _ = SDoc_

text :: String -> SDoc
text _ = SDoc_

instance IsString SDoc where fromString = text

mi_module :: ModIface -> Module
mi_module _ = Module_

showSDocForUser :: DynFlags -> PrintUnqualified -> SDoc -> String
showSDocForUser _ _ _ = ""

alwaysQualify :: PrintUnqualified
alwaysQualify = PrintUnqualified_

tyConsOfType :: Type -> UniqSet TyCon
tyConsOfType _ = UniqSet_

mkCoercionTy :: Coercion -> Type
mkCoercionTy _ = Type_

varType :: Var -> Type
varType _ = Type_

isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind _ = True

splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe _ = Nothing

unitTy :: Type
unitTy = Type_

unitDataConId, voidPrimId, voidArgId, rUNTIME_ERROR_ID :: Id
unitDataConId = Var_
voidPrimId = Var_
voidArgId = Var_
rUNTIME_ERROR_ID = Var_

getOccString :: a -> String
getOccString _ = ""

getName :: a -> Name
getName _ = Name_

tyVarKind :: a -> Kind
tyVarKind _ = Type_

tyConKind :: a -> Kind
tyConKind _ = Type_

tyThingId :: a -> Id
tyThingId _ = Var_

tyThingTyCon :: a -> TyCon
tyThingTyCon _ = TyCon_

boolTy, stringTy, charTy :: Type
boolTy = Type_
stringTy = Type_
charTy = Type_

getOccName :: a -> OccName
getOccName _ = OccName_

trueDataCon, falseDataCon, unitDataCon, charDataCon :: DataCon
trueDataCon = DataCon_
falseDataCon = DataCon_
unitDataCon = DataCon_
charDataCon = DataCon_

boolTyCon, listTyCon, intTyCon, intPrimTyCon, addrPrimTyCon, voidPrimTyCon, unitTyCon :: TyCon
boolTyCon = TyCon_
listTyCon = TyCon_
intTyCon = TyCon_
intPrimTyCon = TyCon_
addrPrimTyCon = TyCon_
voidPrimTyCon = TyCon_
unitTyCon = TyCon_

nonDetEltsUniqSet :: UniqSet a -> [a]
nonDetEltsUniqSet _ = []

normaliseType :: a -> b -> c -> (Coercion, Type)
normaliseType _ _ _ = (Coercion_, Type_)

getTyVar_maybe :: a -> Maybe TyVar
getTyVar_maybe _ = Nothing

splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe _ = Nothing

splitAppTy_maybe :: Type -> Maybe (Type, Type)
splitAppTy_maybe _ = Nothing

splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe _ = Nothing

splitCastTy_maybe :: Type -> Maybe (Type, Coercion)
splitCastTy_maybe _ = Nothing

unwrapNewTyCon_maybe :: TyCon -> Maybe ([TyVar], Type, CoAxiom Unbranched)
unwrapNewTyCon_maybe _ = Nothing

tyConTyVars :: TyCon -> [TyVar]
tyConTyVars _ = []

unionManyUniqSets :: [UniqSet a] -> UniqSet a
unionManyUniqSets _ = UniqSet_

dataConTyCon :: DataCon -> TyCon
dataConTyCon _ = TyCon_

dataConOrigArgTys :: DataCon -> [Type]
dataConOrigArgTys _ = []

dataConInstOrigArgTys :: DataCon -> [Type] -> [Type]
dataConInstOrigArgTys _ _ = []

dataConOrigResTy :: DataCon -> Type
dataConOrigResTy _ = Type_

isAlgTyCon, isTupleTyCon, isFamilyTyCon :: TyCon -> Bool
isAlgTyCon _ = True
isTupleTyCon _ = False
isFamilyTyCon _ = False

isStrLitTy :: Type -> Maybe FastString
isStrLitTy _ = Nothing

infixl 5 $+$
($+$) :: a -> b -> b
_ $+$ x = x

algTyConRhs :: TyCon -> AlgTyConRhs
algTyConRhs _ = AbstractTyCon

mkCharExpr :: Char -> CoreExpr
mkCharExpr c = Lit (LitChar c)

isDefaultAlt :: a -> Bool
isDefaultAlt _ = False

mkListExpr :: Type -> [CoreExpr] -> CoreExpr
mkListExpr _ _ = Lit LitNullAddr

findAlt :: AltCon -> [(AltCon, a, b)] -> Maybe (AltCon, a, b)
findAlt _ _ = Nothing

errorIds :: [Id]
errorIds = []

fvVarList :: FV -> [Var]
fvVarList _ = []

isTyVar :: a -> Bool
isTyVar _ = False

unpackCStringName, unpackCStringFoldrName, buildName :: Name
unpackCStringName = Name_
unpackCStringFoldrName = Name_
buildName = Name_

idDetails :: Id -> IdDetails
idDetails _ = PrimOpId IntEqOp

realIdUnfolding :: Id -> Unfolding
realIdUnfolding _ = Unfolding_

mkDictSelRhs :: Class -> Int -> CoreExpr
mkDictSelRhs _ _ = Lit LitNullAddr

classAllSelIds :: Class -> [Id]
classAllSelIds _ = []

expr_fvs :: CoreExpr -> FV
expr_fvs _ = FV_

maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate _ = Nothing

mkImpossibleExpr :: Type -> CoreExpr
mkImpossibleExpr _ = Lit LitNullAddr

getDynFlags :: Monad m => m DynFlags
getDynFlags = return DynFlags_

mkFastString :: String -> FastString
mkFastString _ = ""

moduleNameString :: ModuleName -> String
moduleNameString _ = ""

findExposedPackageModule :: HscEnv -> ModuleName -> Maybe FastString -> IO FindResult
findExposedPackageModule _ _ _ = return (Found ModLocation_ Module_)

mkModule :: UnitId -> ModuleName -> Module
mkModule _ _ = Module_

initTcInteractive :: HscEnv -> TcM a -> IO (Messages, Maybe a)
initTcInteractive _ _ = pure (Messages_, Nothing)

initIfaceTcRn :: a -> TcRn b
initIfaceTcRn _ = TcM_ StubM_

mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp _ _ = Type_

lookupId :: Monad m => Name -> m Id
lookupId _ = return unitDataConId

lookupTyCon :: Monad m => Name -> m TyCon
lookupTyCon _ = return TyCon_

mkRuntimeErrorApp :: Id -> Type -> String -> CoreExpr
mkRuntimeErrorApp _ _ _ = Lit LitNullAddr

idName :: Id -> Name
idName _ = Name_

showPpr :: DynFlags -> a -> String
showPpr _ _ = ""

lookupThing :: Monad m => Name -> m TyThing
lookupThing _ = return TyThing_

mkTyConTy :: TyCon -> Type
mkTyConTy _ = Type_

mkCoreApps :: CoreExpr -> [CoreExpr] -> CoreExpr
mkCoreApps _ _ = Lit LitNullAddr

mkIntExpr :: DynFlags -> Integer -> CoreExpr
mkIntExpr _ _ = Lit LitNullAddr

mkModuleName :: String -> ModuleName
mkModuleName _ = ModuleName_

noSrcSpan :: SrcSpan
noSrcSpan = SrcSpan_

throwGhcExceptionIO :: GhcException -> IO a
throwGhcExceptionIO = Exception.throwIO

showSDoc :: DynFlags -> SDoc -> String
showSDoc _ _ = ""

cannotFindModule :: DynFlags -> ModuleName -> FindResult -> SDoc
cannotFindModule _ _ _ = SDoc_

hsep :: [SDoc] -> SDoc
hsep _ = SDoc_

panic :: String -> a
panic = error

lookupGRE_RdrName :: RdrName -> GlobalRdrEnv -> [GlobalRdrElt]
lookupGRE_RdrName _ _ = []

gresFromAvails :: Maybe ImportSpec -> [AvailInfo] -> [GlobalRdrElt]
gresFromAvails _ _ = []

mkGlobalRdrEnv :: [GlobalRdrElt] -> GlobalRdrEnv
mkGlobalRdrEnv _ = GlobalRdrEnv_

loadPluginInterface :: SDoc -> Module -> IfM lcl ModIface
loadPluginInterface _ _ = return (ModIface { mi_exports = [] })

getHscEnv :: Monad m => m HscEnv
getHscEnv = return (HscEnv { hsc_dflags = DynFlags_ })

type PackageFamInstEnv       = FamInstEnv

getPackageFamInstEnv :: CoreM PackageFamInstEnv
getPackageFamInstEnv = return FamInstEnv_

mkUnqual :: NameSpace -> FastString -> RdrName
mkUnqual _ _ = RdrName_

bindsOnlyPass :: (CoreProgram -> CoreM CoreProgram) -> ModGuts -> CoreM ModGuts
bindsOnlyPass _ mg = return mg

varName, dataName, tcClsName :: NameSpace
varName = NameSpace_
dataName = NameSpace_
tcClsName = NameSpace_

tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe _ = Nothing


nameOccName :: Name -> OccName
nameOccName _ = undefined

charTyConName :: Name
charTyConName = undefined

noinlineIdName :: Name
noinlineIdName = undefined

nilDataCon :: DataCon
nilDataCon = undefined

dataConWorkId :: DataCon -> Id
dataConWorkId = undefined

thNameToGhcName :: TH.Name -> CoreM (Maybe Name)
thNameToGhcName _ = undefined

showSDocUnsafe :: SDoc -> String
showSDocUnsafe _ = undefined