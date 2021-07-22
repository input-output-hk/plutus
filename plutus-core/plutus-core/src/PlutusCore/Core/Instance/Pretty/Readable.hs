-- | A "readable" Agda-like way to pretty-print PLC entities.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusCore.Core.Instance.Pretty.Readable () where

import           PlutusPrelude

import           PlutusCore.Core.Instance.Pretty.Common ()
import           PlutusCore.Core.Type
import           PlutusCore.Pretty.PrettyConst
import           PlutusCore.Pretty.Readable

import           Control.Monad.Reader
import           Data.Text.Prettyprint.Doc
import           Universe

-- | Pretty-print a binding at the type level.
typeBinderDocM
    :: ( MonadReader env m, HasPrettyConfigReadable env configName
       , PrettyReadableBy configName tyname
       )
    => ((tyname -> Kind a -> Doc ann) -> AnyToDoc (PrettyConfigReadable configName) ann -> Doc ann)
    -> m (Doc ann)
typeBinderDocM k = do
    showKinds <- view $ prettyConfig . pcrShowKinds
    withPrettyAt ToTheRight botFixity $ \prettyBot -> do
        let prettyBind name kind = case showKinds of
                ShowKindsYes -> parens $ prettyBot name <+> "::" <+> prettyBot kind
                ShowKindsNo  -> prettyBot name
        encloseM binderFixity $ k prettyBind prettyBot

instance PrettyBy (PrettyConfigReadable configName) (Kind a) where
    prettyBy = inContextM $ \case
        Type{}          -> "*"
        KindArrow _ k l -> k `arrowPrettyM` l

instance (PrettyReadableBy configName tyname, GShow uni) =>
        PrettyBy (PrettyConfigReadable configName) (Type tyname uni a) where
    prettyBy = inContextM $ \case
        TyApp _ fun arg           -> fun `juxtPrettyM` arg
        TyVar _ name              -> prettyM name
        TyFun _ tyIn tyOut        -> tyIn `arrowPrettyM` tyOut
        TyIFix _ pat arg          ->
            sequenceDocM ToTheRight juxtFixity $ \prettyEl ->
                "ifix" <+> prettyEl pat <+> prettyEl arg
        TyForall _ name kind body ->
            typeBinderDocM $ \prettyBinding prettyBody ->
                "all" <+> prettyBinding name kind <> "." <+> prettyBody body
        TyBuiltin _ builtin       -> unitDocM $ pretty builtin
        TyLam _ name kind body    ->
            typeBinderDocM $ \prettyBinding prettyBody ->
                "\\" <> prettyBinding name kind <+> "->" <+> prettyBody body

instance
        ( PrettyReadableBy configName tyname
        , PrettyReadableBy configName name
        , GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun, Enum fun
        ) => PrettyBy (PrettyConfigReadable configName) (Term tyname name uni fun a) where
    prettyBy = inContextM $ \case
        Constant _ con         -> unitDocM $ pretty con
        Builtin _ bi           -> unitDocM $ pretty bi
        Apply _ fun arg        -> fun `juxtPrettyM` arg
        Var _ name             -> prettyM name
        TyAbs _ name kind body ->
            typeBinderDocM $ \prettyBinding prettyBody ->
                "/\\" <> prettyBinding name kind <+> "->" <+> prettyBody body
        TyInst _ fun ty        ->
            compoundDocM juxtFixity $ \prettyIn ->
                prettyIn ToTheLeft juxtFixity fun <+> braces (prettyIn ToTheRight botFixity ty)
        LamAbs _ name ty body  ->
            compoundDocM binderFixity $ \prettyIn ->
                let prettyBot x = prettyIn ToTheRight botFixity x
                in "\\" <> parens (prettyBot name <+> ":" <+> prettyBot ty) <+> "->" <+> prettyBot body
        Unwrap _ term          ->
            sequenceDocM ToTheRight juxtFixity $ \prettyEl ->
                "unwrap" <+> prettyEl term
        IWrap _ pat arg term   ->
            sequenceDocM ToTheRight juxtFixity $ \prettyEl ->
                "iwrap" <+> prettyEl pat <+> prettyEl arg <+> prettyEl term
        Error _ ty             ->
            compoundDocM juxtFixity $ \prettyIn ->
                "error" <+> braces (prettyIn ToTheRight botFixity ty)

instance PrettyReadableBy configName (Term tyname name uni fun a) =>
        PrettyBy (PrettyConfigReadable configName) (Program tyname name uni fun a) where
    prettyBy = inContextM $ \(Program _ version term) ->
        sequenceDocM ToTheRight juxtFixity $ \prettyEl ->
            "program" <+> pretty version <+> prettyEl term
