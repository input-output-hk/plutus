{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutus.PAB.Webserver.Types where

import           Data.Aeson                              (FromJSON, ToJSON)
import qualified Data.Aeson                              as JSON
import           Data.Map                                (Map)
import           Data.Text.Prettyprint.Doc               (Pretty, pretty, (<+>))
import           GHC.Generics                            (Generic)
import           Ledger                                  (Tx, TxId)
import           Ledger.Index                            (UtxoIndex)
import           Ledger.Slot                             (Slot)
import           Ledger.Value                            (Value)
import           Playground.Types                        (FunctionSchema)
import           Plutus.Contract.Effects                 (ActiveEndpoint, PABReq)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import           Schema                                  (FormSchema)
import           Wallet.Emulator.Wallet                  (Wallet)
import           Wallet.Rollup.Types                     (AnnotatedTx)
import           Wallet.Types                            (ContractInstanceId)

data ContractReport t =
    ContractReport
        { crAvailableContracts   :: [ContractSignatureResponse t]
        , crActiveContractStates :: [(ContractInstanceId, PartiallyDecodedResponse PABReq)]
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

data ChainReport =
    ChainReport
        { transactionMap      :: Map TxId Tx
        , utxoIndex           :: UtxoIndex
        , annotatedBlockchain :: [[AnnotatedTx]]
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

emptyChainReport :: ChainReport
emptyChainReport = ChainReport mempty mempty mempty

data FullReport t =
    FullReport
        { contractReport :: ContractReport t
        , chainReport    :: ChainReport
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

data ContractSignatureResponse t =
    ContractSignatureResponse
        { csrDefinition :: t
        , csrSchemas    :: [FunctionSchema FormSchema]
        }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | Data needed to start a new instance of a contract.
data ContractActivationArgs t =
    ContractActivationArgs
        { caID     :: t -- ^ ID of the contract
        , caWallet :: Wallet -- ^ Wallet that should be used for this instance
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty t => Pretty (ContractActivationArgs t) where
    pretty ContractActivationArgs{caID, caWallet} =
        pretty caID <+> "on" <+> pretty caWallet

-- | Current state of a contract instance
--   (to be sent to external clients)
data ContractInstanceClientState t =
    ContractInstanceClientState
        { cicContract     :: ContractInstanceId
        , cicCurrentState :: PartiallyDecodedResponse ActiveEndpoint
        , cicWallet       :: Wallet
        , cicDefintion    :: t
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- | Status updates for contract instances streamed to client
data InstanceStatusToClient
    = NewObservableState JSON.Value -- ^ The observable state of the contract has changed.
    | NewActiveEndpoints [ActiveEndpoint] -- ^ The set of active endpoints has changed.
    | ContractFinished (Maybe JSON.Value) -- ^ Contract instance is done with an optional error message.
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | Data sent to the client through the combined websocket API
data CombinedWSStreamToClient
    = InstanceUpdate ContractInstanceId InstanceStatusToClient
    | SlotChange Slot -- ^ New slot number
    | WalletFundsChange Wallet Value -- ^ The funds of the wallet have changed
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

-- | Instructions sent to the server through the combined websocket API
data CombinedWSStreamToServer
    = Subscribe (Either ContractInstanceId Wallet)
    | Unsubscribe (Either ContractInstanceId Wallet)
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)
