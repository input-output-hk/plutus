{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | A trace is a sequence of actions by simulated wallets that can be run
--   on the mockchain. This module contains the functions needed to build
--   traces.
module Plutus.Contract.Trace
    ( TraceError(..)
    , EndpointError(..)
    , AsTraceError(..)
    , toNotifyError
    -- * Handle contract requests
    , handleBlockchainQueries
    , handleSlotNotifications
    , handleTimeNotifications
    , handleOwnPubKeyQueries
    , handleCurrentSlotQueries
    , handleCurrentTimeQueries
    , handleUnbalancedTransactions
    , handlePendingTransactions
    , handleUtxoQueries
    , handleTxConfirmedQueries
    , handleAddressChangedAtQueries
    , handleOwnInstanceIdQueries
    -- * Initial distributions of emulated chains
    , InitialDistribution
    , defaultDist
    , defaultDistFor
    -- * Wallets
    , EM.Wallet(..)
    , EM.walletPubKey
    , EM.walletPrivKey
    , allWallets
    , makeTimed
    ) where

import           Control.Lens                         (makeClassyPrisms, preview, review, view)
import           Control.Monad.Freer
import           Control.Monad.Freer.Extras.Log       (LogMessage, LogMsg, LogObserve)
import           Control.Monad.Freer.Reader           (Reader)
import           Control.Monad.Freer.State            (State, gets)
import qualified Data.Aeson.Types                     as JSON
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Text.Prettyprint.Doc            (Pretty, pretty, (<+>))
import           GHC.Generics                         (Generic)

import           Data.Text                            (Text)

import           Plutus.Contract.Effects              (PABReq, PABResp)
import qualified Plutus.Contract.Effects              as E
import           Plutus.Contract.Trace.RequestHandler (RequestHandler (..), RequestHandlerLogMsg, generalise)
import qualified Plutus.Contract.Trace.RequestHandler as RequestHandler

import qualified Ledger.Ada                           as Ada
import           Ledger.Value                         (Value)

import           Plutus.Trace.Emulator.Types          (EmulatedWalletEffects)
import           Wallet.API                           (ChainIndexEffect)
import           Wallet.Effects                       (ContractRuntimeEffect, NodeClientEffect, WalletEffect)
import           Wallet.Emulator                      (EmulatorState, Wallet)
import qualified Wallet.Emulator                      as EM
import qualified Wallet.Emulator.MultiAgent           as EM
import           Wallet.Emulator.Notify               (EmulatorNotifyLogMsg (..))
import           Wallet.Types                         (ContractInstanceId, EndpointDescription (..),
                                                       NotificationError (..))

data EndpointError =
    EndpointNotActive (Maybe Wallet) EndpointDescription
    deriving stock (Eq, Show, Generic)
    deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Pretty EndpointError where
    pretty = \case
        EndpointNotActive w e ->
            "Endpoint not active:" <+> pretty w <+> pretty e

toNotifyError :: ContractInstanceId -> EndpointError -> NotificationError
toNotifyError i = \case
    EndpointNotActive _ e       -> EndpointNotAvailable i e

-- | Error produced while running a trace. Either a contract-specific
--   error (of type 'e'), or an 'EM.AssertionError' from the emulator.
data TraceError e =
    TraceAssertionError EM.AssertionError
    | TContractError e
    | HookError EndpointError
    deriving (Eq, Show)

type InitialDistribution = Map Wallet Value

makeTimed :: Member (State EmulatorState) effs => EmulatorNotifyLogMsg -> Eff effs EM.EmulatorEvent
makeTimed e = do
    emulatorTime <- gets (view (EM.chainState . EM.currentSlot))
    pure $ review (EM.emulatorTimeEvent emulatorTime) (EM.NotificationEvent e)

handleSlotNotifications ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleSlotNotifications =
    generalise (preview E._AwaitSlotReq) E.AwaitSlotResp RequestHandler.handleSlotNotifications

handleTimeNotifications ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleTimeNotifications =
    generalise (preview E._AwaitTimeReq) E.AwaitTimeResp RequestHandler.handleTimeNotifications

handleCurrentSlotQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleCurrentSlotQueries =
    generalise (preview E._CurrentSlotReq) E.CurrentSlotResp RequestHandler.handleCurrentSlot

handleCurrentTimeQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleCurrentTimeQueries =
    generalise (preview E._CurrentTimeReq) E.CurrentTimeResp RequestHandler.handleCurrentTime

handleBlockchainQueries ::
    RequestHandler
        (Reader ContractInstanceId ': ContractRuntimeEffect ': EmulatedWalletEffects)
        PABReq
        PABResp
handleBlockchainQueries =
    handleUnbalancedTransactions
    <> handlePendingTransactions
    <> handleUtxoQueries
    <> handleTxConfirmedQueries
    <> handleOwnPubKeyQueries
    <> handleAddressChangedAtQueries
    <> handleOwnInstanceIdQueries
    <> handleSlotNotifications
    <> handleCurrentSlotQueries
    <> handleTimeNotifications
    <> handleCurrentTimeQueries

handleUnbalancedTransactions ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleUnbalancedTransactions =
    generalise
        (preview E._BalanceTxReq)
        (E.BalanceTxResp . either E.BalanceTxFailed E.BalanceTxSuccess)
        RequestHandler.handleUnbalancedTransactions

-- | Submit the wallet's pending transactions to the blockchain
--   and inform all wallets about new transactions and respond to
--   UTXO queries
handlePendingTransactions ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member WalletEffect effs
    , Member ChainIndexEffect effs
    )
    => RequestHandler effs PABReq PABResp
handlePendingTransactions =
    generalise
        (preview E._WriteBalancedTxReq)
        (E.WriteBalancedTxResp . either E.WriteBalancedTxFailed E.WriteBalancedTxSuccess)
        RequestHandler.handlePendingTransactions

-- | Look at the "utxo-at" requests of the contract and respond to all of them
--   with the current UTXO set at the given address.
handleUtxoQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member ChainIndexEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleUtxoQueries =
    generalise (preview E._UtxoAtReq) E.UtxoAtResp RequestHandler.handleUtxoQueries

handleTxConfirmedQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member ChainIndexEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleTxConfirmedQueries =
    generalise (preview E._AwaitTxConfirmedReq) (E.AwaitTxConfirmedResp . fmap E.unTxConfirmed) RequestHandler.handleTxConfirmedQueries

handleAddressChangedAtQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    , Member ChainIndexEffect effs
    , Member NodeClientEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleAddressChangedAtQueries =
    generalise (preview E._AddressChangeReq) E.AddressChangeResp RequestHandler.handleAddressChangedAtQueries

handleOwnPubKeyQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member WalletEffect effs
    )
    => RequestHandler effs PABReq PABResp
handleOwnPubKeyQueries =
    generalise (preview E._OwnPublicKeyReq) E.OwnPublicKeyResp RequestHandler.handleOwnPubKey

handleOwnInstanceIdQueries ::
    ( Member (LogObserve (LogMessage Text)) effs
    , Member (Reader ContractInstanceId) effs
    )
    => RequestHandler effs PABReq PABResp
handleOwnInstanceIdQueries =
    generalise (preview E._OwnContractInstanceIdReq) E.OwnContractInstanceIdResp RequestHandler.handleOwnInstanceIdQueries

-- | The wallets used in mockchain simulations by default. There are
--   ten wallets because the emulator comes with ten private keys.
allWallets :: [EM.Wallet]
allWallets = EM.Wallet <$> [1 .. 10]

defaultDist :: InitialDistribution
defaultDist = defaultDistFor allWallets

defaultDistFor :: [EM.Wallet] -> InitialDistribution
defaultDistFor wallets = Map.fromList $ zip wallets (repeat (Ada.lovelaceValueOf 100_000_000))

makeClassyPrisms ''TraceError

instance EM.AsAssertionError (TraceError e) where
    _AssertionError = _TraceAssertionError
