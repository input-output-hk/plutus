-- | A "classic" (i.e. as seen in the specification) way to pretty-print Untyped Plutus Core terms.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module UntypedPlutusCore.Core.Instance.Pretty.Classic () where

import           PlutusPrelude

import           UntypedPlutusCore.Core.Type

import           PlutusCore.Core.Instance.Pretty.Common ()
import           PlutusCore.Pretty.Classic
import           PlutusCore.Pretty.PrettyConst

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Custom
import           Universe

instance
        ( PrettyClassicBy configName name
        , GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun
        ) => PrettyBy (PrettyConfigClassic configName) (Term name uni fun a) where
    prettyBy config = go where
        go (Constant _ val)     = parens' $ "con" </> prettyTypeOf val </> pretty val  -- NB: actually calls prettyConst
        go (Builtin _ bi)       = parens' $ "builtin" </> pretty bi
        go (Var _ name)         = prettyName name
        go (LamAbs _ name body) = parens' $ "lam" </> vsep' [prettyName name, go body]
        go (Apply _ fun arg)    = brackets' $ vsep' [go fun, go arg]
        go (Delay _ term)       = parens' ("delay" </> go term)
        go (Force _ term)       = parens' ("force" </> go term)
        go (Error _)            = parens' "error"

        prettyName :: PrettyClassicBy configName n => n -> Doc ann
        prettyName = prettyBy config

        prettyTypeOf :: GShow t => Some (ValueOf t) -> Doc ann
        prettyTypeOf (Some (ValueOf uni _ )) = pretty $ SomeTypeIn uni

instance PrettyClassicBy configName (Term name uni fun a) =>
        PrettyBy (PrettyConfigClassic configName) (Program name uni fun a) where
    prettyBy config (Program _ version term) = parens' $ "program" <+> pretty version <//> prettyBy config term

instance
        (GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun

        ) => PrettyBy (PrettyConfigClassic configName) (ETerm uni fun) where
    prettyBy _ = go where
        go (EConstant val)     = parens' $ "con" </> prettyTypeOf val </> pretty val  -- NB: actually calls prettyConst
        go (EBuiltin bi)       = parens' $ "builtin" </> pretty bi
        go (EVar name)         = pretty name
        go (ELamAbs name body) = parens' $ "lam" </> vsep' [pretty name, go body]
        go (EApply fun arg)    = brackets' $ vsep' [go fun, go arg]
        go (EDelay term)       = parens' ("delay" </> go term)
        go (EForce term)       = parens' ("force" </> go term)
        go EError              = parens' "error"

        prettyTypeOf :: GShow t => Some (ValueOf t) -> Doc ann
        prettyTypeOf (Some (ValueOf uni _ )) = pretty $ SomeTypeIn uni

instance PrettyClassicBy configName (ETerm uni fun) =>
        PrettyBy (PrettyConfigClassic configName) (EProgram uni fun) where
    prettyBy config (EProgram version term) = parens' $ "program" <+> pretty version <//> prettyBy config term
