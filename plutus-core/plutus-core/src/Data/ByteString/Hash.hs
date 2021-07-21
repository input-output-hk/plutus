-- | Hash functions for lazy [[Data.ByteString.ByteString]]s
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Hash
    ( sha2
    , sha3
    , blake2b
    ) where

import           Cardano.Crypto.Hash.Blake2b
import           Cardano.Crypto.Hash.Class
import           Cardano.Crypto.Hash.SHA256
import           Cardano.Crypto.Hash.SHA3_256
import qualified Data.ByteString              as BS
import           Data.Proxy

-- | Hash a [[BSL.ByteString]] using the SHA-256 hash function.
sha2 :: BS.ByteString -> BS.ByteString
sha2 = digest (Proxy @SHA256)

-- | Hash a [[BSL.ByteString]] using the SHA3-256 hash function.
sha3 :: BS.ByteString -> BS.ByteString
sha3 = digest (Proxy @SHA3_256)

-- | Hash a [[BSL.ByteString]] using the Blake2B-256 hash function.
blake2b :: BS.ByteString -> BS.ByteString
blake2b = digest (Proxy @Blake2b_256)
