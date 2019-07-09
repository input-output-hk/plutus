{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.PlutusCore.Lexer.Type
    ( BuiltinName (..)
    , DynamicBuiltinName (..)
    , StagedBuiltinName (..)
    , Version (..)
    , Keyword (..)
    , Special (..)
    , Token (..)
    , prettyBytes
    , allBuiltinNames
    , defaultVersion
    ) where

import           Language.PlutusCore.Name
import           PlutusPrelude

import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Text                          as T
import           Data.Text.Prettyprint.Doc.Internal (Doc (Text))
import           Language.Haskell.TH.Syntax         (Lift)
import           Numeric                            (showHex)

-- | Builtin functions
data BuiltinName
    = AddInteger
    | SubtractInteger
    | MultiplyInteger
    | DivideInteger
    | QuotientInteger
    | RemainderInteger
    | ModInteger
    | LessThanInteger
    | LessThanEqInteger
    | GreaterThanInteger
    | GreaterThanEqInteger
    | EqInteger
    | Concatenate
    | TakeByteString
    | DropByteString
    | SHA2
    | SHA3
    | VerifySignature
    | EqByteString
    | LtByteString
    | GtByteString
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, Lift)

-- | The type of dynamic built-in functions. I.e. functions that exist on certain chains and do
-- not exist on others. Each 'DynamicBuiltinName' has an associated type and operational semantics --
-- this allows to type check and evaluate dynamic built-in names just like static ones.
newtype DynamicBuiltinName = DynamicBuiltinName
    { unDynamicBuiltinName :: T.Text  -- ^ The name of a dynamic built-in name.
    } deriving (Show, Eq, Ord, Generic)
      deriving newtype (NFData, Lift)

-- | Either a 'BuiltinName' (known statically) or a 'DynamicBuiltinName' (known dynamically).
data StagedBuiltinName
    = StaticStagedBuiltinName  BuiltinName
    | DynamicStagedBuiltinName DynamicBuiltinName
    deriving (Show, Eq, Generic, NFData, Lift)

-- | Version of Plutus Core to be used for the program.
data Version a
    = Version a Natural Natural Natural
    deriving (Show, Eq, Functor, Generic, NFData, Lift)

-- | A keyword in Plutus Core.
data Keyword
    = KwAbs
    | KwLam
    | KwIFix
    | KwFun
    | KwAll
    | KwByteString
    | KwInteger
    | KwType
    | KwProgram
    | KwCon
    | KwIWrap
    | KwBuiltin
    | KwUnwrap
    | KwError
    deriving (Show, Eq, Generic, NFData)

-- | A special character. This type is only used internally between the lexer
-- and the parser.
data Special
    = OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | Dot
    | Exclamation
    | OpenBrace
    | CloseBrace
    deriving (Show, Eq, Generic, NFData)

-- | A token generated by the lexer.
data Token a
    = LexName { loc        :: a
              , name       :: T.Text
              , identifier :: Unique -- ^ A 'Unique' assigned to the identifier during lexing.
              }
    | LexInt { loc :: a, tkInt :: Integer }
    | LexBS { loc :: a, tkBytestring :: BSL.ByteString }
    | LexBuiltin { loc :: a, tkBuiltin :: BuiltinName }
    | LexNat { loc :: a, tkNat :: Natural }
    | LexKeyword { loc :: a, tkKeyword :: Keyword }
    | LexSpecial { loc :: a, tkSpecial :: Special }
    | EOF { loc :: a }
    deriving (Show, Eq, Generic, NFData)

asBytes :: Word8 -> Doc a
asBytes x = Text 2 $ T.pack $ addLeadingZero $ showHex x mempty
    where addLeadingZero :: String -> String
          addLeadingZero
              | x < 16    = ('0' :)
              | otherwise = id

prettyBytes :: BSL.ByteString -> Doc a
prettyBytes b = "#" <> fold (asBytes <$> BSL.unpack b)
instance Pretty Special where
    pretty OpenParen    = "("
    pretty CloseParen   = ")"
    pretty OpenBracket  = "["
    pretty CloseBracket = "]"
    pretty Dot          = "."
    pretty Exclamation  = "!"
    pretty OpenBrace    = "{"
    pretty CloseBrace   = "}"

instance Pretty Keyword where
    pretty KwAbs        = "abs"
    pretty KwLam        = "lam"
    pretty KwIFix       = "ifix"
    pretty KwFun        = "fun"
    pretty KwAll        = "forall"
    pretty KwByteString = "bytestring"
    pretty KwInteger    = "integer"
    pretty KwType       = "type"
    pretty KwProgram    = "program"
    pretty KwCon        = "con"
    pretty KwIWrap      = "iwrap"
    pretty KwBuiltin    = "builtin"
    pretty KwUnwrap     = "unwrap"
    pretty KwError      = "error"

instance Pretty (Token a) where
    pretty (LexName _ n _)   = pretty n
    pretty (LexInt _ i)      = pretty i
    pretty (LexNat _ n)      = pretty n
    pretty (LexBS _ bs)      = prettyBytes bs
    pretty (LexBuiltin _ bn) = pretty bn
    pretty (LexKeyword _ kw) = pretty kw
    pretty (LexSpecial _ s)  = pretty s
    pretty EOF{}             = mempty

instance Pretty BuiltinName where
    pretty AddInteger           = "addInteger"
    pretty SubtractInteger      = "subtractInteger"
    pretty MultiplyInteger      = "multiplyInteger"
    pretty DivideInteger        = "divideInteger"
    pretty QuotientInteger      = "quotientInteger"
    pretty ModInteger           = "modInteger"
    pretty RemainderInteger     = "remainderInteger"
    pretty LessThanInteger      = "lessThanInteger"
    pretty LessThanEqInteger    = "lessThanEqualsInteger"
    pretty GreaterThanInteger   = "greaterThanInteger"
    pretty GreaterThanEqInteger = "greaterThanEqualsInteger"
    pretty EqInteger            = "equalsInteger"
    pretty Concatenate          = "concatenate"
    pretty TakeByteString       = "takeByteString"
    pretty DropByteString       = "dropByteString"
    pretty EqByteString         = "equalsByteString"
    pretty LtByteString         = "lessThanByteString"
    pretty GtByteString         = "greaterThanByteString"
    pretty SHA2                 = "sha2_256"
    pretty SHA3                 = "sha3_256"
    pretty VerifySignature      = "verifySignature"

instance Pretty DynamicBuiltinName where
    pretty (DynamicBuiltinName n) = pretty n

instance Pretty StagedBuiltinName where
    pretty (StaticStagedBuiltinName  n) = pretty n
    pretty (DynamicStagedBuiltinName n) = pretty n

instance Pretty (Version a) where
    pretty (Version _ i j k) = pretty i <> "." <> pretty j <> "." <> pretty k

-- | The list of all 'BuiltinName's.
allBuiltinNames :: [BuiltinName]
allBuiltinNames = [minBound .. maxBound]
-- The way it's defined ensures that it's enough to add a new built-in to 'BuiltinName' and it'll be
-- automatically handled by tests and other stuff that deals with all built-in names at once.

-- | The default version of Plutus Core supported by this library.
defaultVersion :: a -> Version a
defaultVersion a = Version a 1 0 0
