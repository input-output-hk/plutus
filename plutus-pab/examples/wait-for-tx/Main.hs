{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-| A plutus app that waits for a transaction to be confirmed and then exits.
-}
module Main(main) where

import           Data.Proxy             (Proxy (..))
import qualified Data.Text              as T

import           Data.Bifunctor         (first)
import           Ledger                 (TxId)
import           Plutus.Contract
import           Plutus.PAB.ContractCLI (commandLineApp')

main :: IO ()
main = commandLineApp' (Proxy @WaitForTxSchema) $ first (T.pack . show) waitForTx

type WaitForTxSchema = Endpoint "tx-id" TxId

waitForTx :: Contract () WaitForTxSchema ContractError (Waited ())
waitForTx = endpoint @"tx-id" $ \txid -> do
    logInfo @String $ "Waiting for transaction " <> show txid
    awaitTxConfirmed txid
    logInfo @String "CONFIRMED"
