{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module PlutusIR.Core.Instance.Pretty () where

import           PlutusPrelude

import qualified PlutusCore                       as PLC
import           PlutusCore.Flat                  ()
import qualified PlutusCore.Pretty                as PLC

import           PlutusIR.Core.Type


import           Data.Text.Prettyprint.Doc.Custom

-- Pretty-printing

instance ( PLC.PrettyClassicBy configName tyname
         , PLC.PrettyClassicBy configName name
         , PLC.GShow uni, uni `PLC.Everywhere` PLC.PrettyConst
         ) => PrettyBy (PLC.PrettyConfigClassic configName) (VarDecl tyname name uni fun a) where
    prettyBy config (VarDecl _ n ty) = parens' ("vardecl" </> vsep' [prettyBy config n, prettyBy config ty])

instance (PLC.PrettyClassicBy configName tyname) =>
        PrettyBy (PLC.PrettyConfigClassic configName) (TyVarDecl tyname a) where
    prettyBy config (TyVarDecl _ n ty) = parens' ("tyvardecl" </> vsep' [prettyBy config n, prettyBy config ty])

instance PrettyBy (PLC.PrettyConfigClassic configName) Recursivity where
    prettyBy _ = \case
        NonRec -> parens' "nonrec"
        Rec    -> parens' "rec"

instance PrettyBy (PLC.PrettyConfigClassic configName) Strictness where
    prettyBy _ = \case
        NonStrict -> parens' "nonstrict"
        Strict    -> parens' "strict"

instance ( PLC.PrettyClassicBy configName tyname
         , PLC.PrettyClassicBy configName name
         , PLC.GShow uni, uni `PLC.Everywhere` PLC.PrettyConst
         ) => PrettyBy (PLC.PrettyConfigClassic configName) (Datatype tyname name uni fun a) where
    prettyBy config (Datatype _ ty tyvars destr constrs) = parens' ("datatype" </> vsep' [
                                                                         prettyBy config ty,
                                                                         vsep' $ fmap (prettyBy config) tyvars,
                                                                         prettyBy config destr,
                                                                         vsep' $ fmap (prettyBy config) constrs])

instance ( PLC.PrettyClassicBy configName tyname
         , PLC.PrettyClassicBy configName name
         , PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
         , Pretty fun, Enum fun
         ) => PrettyBy (PLC.PrettyConfigClassic configName) (Binding tyname name uni fun a) where
    prettyBy config = \case
        TermBind _ s d t -> parens' ("termbind" </> vsep' [prettyBy config s, prettyBy config d, prettyBy config t])
        TypeBind _ d ty  -> parens' ("typebind" </> vsep' [prettyBy config d, prettyBy config ty])
        DatatypeBind _ d -> parens' ("datatypebind" </> prettyBy config d)

instance ( PLC.PrettyClassicBy configName tyname
         , PLC.PrettyClassicBy configName name
         , PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
         , Pretty fun, Enum fun
         ) => PrettyBy (PLC.PrettyConfigClassic configName) (Term tyname name uni fun a) where
    prettyBy config = \case
        Let _ r bs t -> parens' ("let" </> vsep' [prettyBy config r, vsep' . toList $ fmap (prettyBy config) bs, prettyBy config t])
        Var _ n -> prettyBy config n
        TyAbs _ tn k t -> parens' ("abs" </> vsep' [prettyBy config tn, prettyBy config k, prettyBy config t])
        LamAbs _ n ty t -> parens' ("lam" </> vsep' [prettyBy config n, prettyBy config ty, prettyBy config t])
        Apply _ t1 t2 -> brackets' (vsep' [prettyBy config t1, prettyBy config t2])
        Constant _ c -> parens' ("con" </> prettyTypeOf c </> pretty c)
        Builtin _ bi -> parens' ("builtin" </> pretty bi)
        TyInst _ t ty -> braces' (vsep' [prettyBy config t, prettyBy config ty])
        Error _ ty -> parens' ("error" </> prettyBy config ty)
        IWrap _ ty1 ty2 t -> parens' ("iwrap" </> vsep' [ prettyBy config ty1, prettyBy config ty2, prettyBy config t ])
        Unwrap _ t -> parens' ("unwrap" </> prettyBy config t)

        where prettyTypeOf :: PLC.GShow t => PLC.Some (PLC.ValueOf t) -> Doc ann
              prettyTypeOf (PLC.Some (PLC.ValueOf uni _ )) = pretty $ PLC.SomeTypeIn uni


instance ( PLC.PrettyClassicBy configName tyname
         , PLC.PrettyClassicBy configName name
         , PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
         , Pretty fun, Enum fun
         ) => PrettyBy (PLC.PrettyConfigClassic configName) (Program tyname name uni fun a) where
    prettyBy config (Program _ t) = parens' ("program" </> prettyBy config t)

-- See note [Default pretty instances for PLC]
instance (PLC.PrettyClassic tyname) =>
    Pretty (TyVarDecl tyname a) where
    pretty = PLC.prettyClassicDef

instance ( PLC.PrettyClassic tyname
         , PLC.PrettyClassic name
         , PLC.GShow uni, uni `PLC.Everywhere` PLC.PrettyConst
         ) => Pretty (VarDecl tyname name uni fun a) where
    pretty = PLC.prettyClassicDef

instance ( PLC.PrettyClassic tyname
         , PLC.PrettyClassic name
         , PLC.GShow uni, uni `PLC.Everywhere` PLC.PrettyConst
         ) => Pretty (Datatype tyname name uni fun a) where
    pretty = PLC.prettyClassicDef

instance ( PLC.PrettyClassic tyname
         , PLC.PrettyClassic name
         , PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
         , Pretty fun, Enum fun
         ) => Pretty (Binding tyname name uni fun a) where
    pretty = PLC.prettyClassicDef

instance ( PLC.PrettyClassic tyname
         , PLC.PrettyClassic name
         , PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
         , Pretty fun, Enum fun
         ) => Pretty (Term tyname name uni fun a) where
    pretty = PLC.prettyClassicDef

instance ( PLC.PrettyClassic tyname
         , PLC.PrettyClassic name
         , PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
         , Pretty fun, Enum fun
         ) => Pretty (Program tyname name uni fun a) where
    pretty = PLC.prettyClassicDef
