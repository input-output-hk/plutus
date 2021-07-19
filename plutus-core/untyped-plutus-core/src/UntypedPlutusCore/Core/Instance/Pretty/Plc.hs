{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module UntypedPlutusCore.Core.Instance.Pretty.Plc () where

import           PlutusPrelude

import           UntypedPlutusCore.Core.Instance.Pretty.Classic  ()
import           UntypedPlutusCore.Core.Instance.Pretty.Readable ()
import           UntypedPlutusCore.Core.Type

import           PlutusCore.Pretty.Plc

deriving via PrettyAny (Term name uni fun ann)
    instance DefaultPrettyPlcStrategy (Term name uni fun ann) =>
        PrettyBy PrettyConfigPlc (Term name uni fun ann)
deriving via PrettyAny (Program name uni fun ann)
    instance DefaultPrettyPlcStrategy (Program name uni fun ann) =>
        PrettyBy PrettyConfigPlc (Program name uni fun ann)

deriving via PrettyAny (ETerm uni fun)
    instance DefaultPrettyPlcStrategy (ETerm uni fun) =>
        PrettyBy PrettyConfigPlc (ETerm uni fun)
deriving via PrettyAny (EProgram uni fun)
    instance DefaultPrettyPlcStrategy (EProgram uni fun) =>
        PrettyBy PrettyConfigPlc (EProgram uni fun)
