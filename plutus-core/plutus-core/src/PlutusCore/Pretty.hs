module PlutusCore.Pretty
    (
    -- * Basic types and functions
      Doc
    , Pretty (..)
    , PrettyBy (..)
    , IgnorePrettyConfig (..)
    , AttachPrettyConfig (..)
    , Render (..)
    , display
    , displayBy
    -- * Defaults
    , prettyPlcDef
    , displayPlcDef
    , prettyPlcDebug
    , displayPlcDebug
    -- * Global configuration
    , CondensedErrors (..)
    , PrettyConfigPlcOptions (..)
    , PrettyConfigPlcStrategy (..)
    , PrettyConfigPlc (..)
    , PrettyPlc
    , defPrettyConfigPlcOptions
    , defPrettyConfigPlcClassic
    , debugPrettyConfigPlcClassic
    , defPrettyConfigPlcReadable
    , debugPrettyConfigPlcReadable
    -- * Custom functions for PLC types.
    , prettyPlcClassicDef
    , prettyPlcClassicDebug
    , prettyPlcReadableDef
    , prettyPlcReadableDebug
    , prettyPlcCondensedErrorBy
    , displayPlcCondensedErrorClassic
    -- * Names
    , PrettyConfigName (..)
    , HasPrettyConfigName (..)
    , defPrettyConfigName
    , debugPrettyConfigName
    -- * Classic view
    , PrettyConfigClassic (..)
    , PrettyClassicBy
    , PrettyClassic
    , prettyClassicDef
    , prettyClassicDebug
    -- * Readable view
    , ShowKinds (..)
    , PrettyConfigReadable (..)
    , PrettyReadableBy
    , PrettyReadable
    , topPrettyConfigReadable
    , botPrettyConfigReadable
    -- * Utils
    , prettyBytes
    , ConstConfig (..)
    , PrettyConst
    , prettyConst
    ) where

import           PlutusCore.Pretty.Classic
import           PlutusCore.Pretty.ConfigName
import           PlutusCore.Pretty.Default
import           PlutusCore.Pretty.Plc
import           PlutusCore.Pretty.PrettyConst
import           PlutusCore.Pretty.Readable
import           PlutusCore.Pretty.Utils

import           Text.Pretty
import           Text.PrettyBy
