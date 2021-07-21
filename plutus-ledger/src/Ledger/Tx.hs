module Ledger.Tx
    ( module Export
    , addSignature
    , pubKeyTxOut
    , scriptTxOut
    , scriptTxOut'
    ) where

import           Control.Lens
import           Ledger.Address         (pubKeyAddress, scriptAddress)
import           Ledger.Crypto          (PrivateKey, PubKey, signTx, toPublicKey)
import           Ledger.Scripts         (Datum, Validator, datumHash)
import           Plutus.V1.Ledger.Tx    as Export
import           Plutus.V1.Ledger.Value

-- | Create a transaction output locked by a validator script hash
--   with the given data script attached.
scriptTxOut' :: Value -> Address -> Datum -> TxOut
scriptTxOut' v a ds = TxOut a v (Just (datumHash ds))

-- | Create a transaction output locked by a validator script and with the given data script attached.
scriptTxOut :: Value -> Validator -> Datum -> TxOut
scriptTxOut v vs = scriptTxOut' v (scriptAddress vs)

-- | Create a transaction output locked by a public key.
pubKeyTxOut :: Value -> PubKey -> TxOut
pubKeyTxOut v pk = TxOut (pubKeyAddress pk) v Nothing

-- | Sign the transaction with a 'PrivateKey' and add the signature to the
--   transaction's list of signatures.
addSignature :: PrivateKey -> Tx -> Tx
addSignature privK tx = tx & signatures . at pubK ?~ sig where
    sig = signTx (txId tx) privK
    pubK = toPublicKey privK

