{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module PlutusIR.Error
    ( Error (..)
    , PLC.AsTypeError (..)
    , PLC.TypeError
    , AsTypeErrorExt (..)
    , AsError (..)
    , TypeErrorExt (..)
    , PLC.Normalized (..)
    ) where

import qualified PlutusCore                as PLC
import qualified PlutusCore.Pretty         as PLC
import           PlutusPrelude

import qualified PlutusIR                  as PIR

import           Control.Lens

import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc as PP
import           ErrorCode

data TypeErrorExt uni ann =
      MalformedDataConstrResType
         ann
         -- the expected constructor's type
         (PLC.Type PLC.TyName uni ann)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
makeClassyPrisms ''TypeErrorExt

instance HasErrorCode (TypeErrorExt _a _b) where
  errorCode MalformedDataConstrResType {} = ErrorCode 1

data Error uni fun a = CompilationError a T.Text -- ^ A generic compilation error.
                     | UnsupportedError a T.Text -- ^ An error relating specifically to an unsupported feature.
                     | PLCError (PLC.Error uni fun a) -- ^ An error from running some PLC function, lifted into this error type for convenience.
                     | PLCTypeError (PLC.TypeError (PIR.Term PIR.TyName PIR.Name uni fun ()) uni fun a)
                     | PIRTypeError (TypeErrorExt uni a)
makeClassyPrisms ''Error

instance HasErrorCode (Error _a _b _c) where
   errorCode UnsupportedError {} = ErrorCode 3
   errorCode CompilationError {} = ErrorCode 2
   errorCode (PIRTypeError e)    = errorCode e
   errorCode (PLCTypeError e)    = errorCode e
   errorCode (PLCError e)        = errorCode e


instance PLC.AsTypeError (Error uni fun a) (PIR.Term PIR.TyName PIR.Name uni fun ()) uni fun a where
    _TypeError = _PLCTypeError

instance AsTypeErrorExt (Error uni fun a) uni a where
    _TypeErrorExt = _PIRTypeError

instance PLC.AsFreeVariableError (Error uni fun a) where
    _FreeVariableError = _PLCError . PLC._FreeVariableError

-- Pretty-printing
------------------

type PrettyUni uni ann =
    (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty ann)

instance (PrettyUni uni ann) => PrettyBy PLC.PrettyConfigPlc (TypeErrorExt uni ann) where
    prettyBy config (MalformedDataConstrResType ann expType) =
        vsep ["The result-type of a dataconstructor is malformed at location" <+> PP.pretty ann
             , "The expected result-type is:" <+> prettyBy config expType]

-- show via pretty, for printing as SomeExceptions
instance (PrettyUni uni ann, Pretty fun, Enum fun) => Show (Error uni fun ann) where
    show = show . PP.pretty

-- FIXME: we get rid of this when our TestLib stops using rethrow
deriving anyclass instance
    (PrettyUni uni ann, Typeable uni, Typeable fun, Typeable ann, Pretty fun, Enum fun) => Exception (Error uni fun ann)

instance
        (Pretty ann, Pretty fun, Enum fun,
        PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst
        ) => Pretty (Error uni fun ann) where
    pretty = PLC.prettyPlcClassicDef


instance (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, Pretty fun, Enum fun, Pretty ann) =>
            PrettyBy PLC.PrettyConfigPlc (Error uni fun ann) where
     prettyBy config er = PP.pretty (errorCode er) <> ":" <+> case er of
        CompilationError x e -> "Error during compilation:" <+> PP.pretty e <> "(" <> PP.pretty x <> ")"
        UnsupportedError x e -> "Unsupported construct:" <+> PP.pretty e <+> "(" <> PP.pretty x <> ")"
        PLCError e           -> PP.vsep [ "Error from the PLC compiler:", PLC.prettyBy config e ]
        PLCTypeError e       -> PP.vsep ["Error during PIR typechecking:" , PLC.prettyBy config e ]
        PIRTypeError e       -> PP.vsep ["Error during PIR typechecking:" , PLC.prettyBy config e ]
