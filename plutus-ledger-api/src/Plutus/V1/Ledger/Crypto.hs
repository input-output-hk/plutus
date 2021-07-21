{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.V1.Ledger.Crypto(
    PubKey(..)
    , PubKeyHash(..)
    , PrivateKey(..)
    , Signature(..)
    ) where

import           Codec.Serialise.Class     (Serialise)
import           Control.DeepSeq           (NFData)
import           Control.Newtype.Generics  (Newtype)
import           Data.Aeson                (FromJSON (parseJSON), FromJSONKey, FromJSONKeyFunction (FromJSONKeyValue),
                                            ToJSON (toJSON), ToJSONKey, ToJSONKeyFunction (ToJSONKeyValue),
                                            genericParseJSON, genericToJSON, (.:))
import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Extras         as JSON
import qualified Data.ByteString           as BS
import           Data.Hashable             (Hashable)
import           Data.String
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)
import           Plutus.V1.Ledger.Bytes    (LedgerBytes (..))
import           Plutus.V1.Ledger.Orphans  ()
import qualified PlutusTx
import qualified PlutusTx.Builtins         as Builtins
import           PlutusTx.Lift             (makeLift)
import qualified PlutusTx.Prelude          as P

-- | A cryptographic public key.
newtype PubKey = PubKey { getPubKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (Newtype, ToJSON, FromJSON, NFData)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
makeLift ''PubKey

instance ToJSONKey PubKey where
  toJSONKey = ToJSONKeyValue (genericToJSON JSON.defaultOptions) JSON.toEncoding

instance FromJSONKey PubKey where
  fromJSONKey = FromJSONKeyValue (genericParseJSON JSON.defaultOptions)

-- | The hash of a public key. This is frequently used to identify the public key, rather than the key itself.
newtype PubKeyHash = PubKeyHash { getPubKeyHash :: BS.ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey, NFData)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData, Hashable)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
makeLift ''PubKeyHash

-- | A cryptographic private key.
newtype PrivateKey = PrivateKey { getPrivateKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData)
    deriving (Show, Pretty) via LedgerBytes

makeLift ''PrivateKey

-- | A message with a cryptographic signature.
newtype Signature = Signature { getSignature :: Builtins.ByteString }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (P.Eq, P.Ord, Serialise, PlutusTx.IsData, NFData)
    deriving (Show, Pretty) via LedgerBytes

instance ToJSON Signature where
  toJSON signature =
    JSON.object
      [ ( "getSignature"
        , JSON.String .
          JSON.encodeByteString .
          getSignature $
          signature)
      ]

instance FromJSON Signature where
  parseJSON =
    JSON.withObject "Signature" $ \object -> do
      raw <- object .: "getSignature"
      bytes <- JSON.decodeByteString raw
      pure . Signature $ bytes

makeLift ''Signature
