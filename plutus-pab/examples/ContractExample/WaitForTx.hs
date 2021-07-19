{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module ContractExample.WaitForTx (
    waitForTx
    , WaitForTxSchema
    ) where

import           Ledger          (TxId)
import           Plutus.Contract (Contract, ContractError, Endpoint, awaitTxConfirmed, endpoint, logInfo)

type WaitForTxSchema = Endpoint "tx-id" TxId

waitForTx :: Contract () WaitForTxSchema ContractError ()
waitForTx = do
    txid <- endpoint @"tx-id"
    logInfo @String $ "Waiting for transaction " <> show txid
    awaitTxConfirmed txid
    logInfo @String "CONFIRMED"
