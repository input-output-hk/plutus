{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Wallet.Emulator.NodeClient where

import           Control.Lens                   hiding (index)
import           Control.Monad.Freer
import           Control.Monad.Freer.Extras.Log (LogMsg, logInfo)
import           Control.Monad.Freer.State
import           Control.Monad.Freer.TH
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc      hiding (annotate)
import           GHC.Generics                   (Generic)
import           Ledger
import qualified Ledger.AddressMap              as AM
import           Wallet.Effects                 (NodeClientEffect (..))
import           Wallet.Emulator.Chain

data NodeClientEvent =
    TxSubmit TxId Value
    -- ^ A transaction has been added to the pool of pending transactions. The value is the fee of the transaction.
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty NodeClientEvent where
    pretty (TxSubmit tx _) = "TxSubmit:" <+> pretty tx

makePrisms ''NodeClientEvent

data NodeClientState = NodeClientState {
    _clientSlot  :: Slot,
    _clientIndex :: AM.AddressMap
    -- ^ Full index
} deriving stock (Show, Eq)

emptyNodeClientState :: NodeClientState
emptyNodeClientState = NodeClientState (Slot 0) mempty

makeLenses ''NodeClientState

data ChainClientNotification = BlockValidated Block | SlotChanged Slot
    deriving (Show, Eq)

data NodeClientControlEffect r where
    ClientNotify :: ChainClientNotification -> NodeClientControlEffect ()
makeEffect ''NodeClientControlEffect

type NodeClientEffs = '[ChainEffect, State NodeClientState, LogMsg NodeClientEvent]

handleNodeControl
    :: (Members NodeClientEffs effs)
    => Eff (NodeClientControlEffect ': effs) ~> Eff effs
handleNodeControl = interpret $ \case
    ClientNotify (BlockValidated blk) -> modify $ \s ->
            s & clientIndex %~ (\am -> foldl (\am' t -> AM.updateAllAddresses t am') am blk)
    ClientNotify (SlotChanged sl) -> modify (set clientSlot sl)

handleNodeClient
    :: (Members NodeClientEffs effs)
    => Eff (NodeClientEffect ': effs) ~> Eff effs
handleNodeClient = interpret $ \case
    PublishTx tx        -> queueTx tx >> logInfo (TxSubmit (txId tx) (txFee tx))
    GetClientSlot       -> gets _clientSlot
    GetClientSlotConfig -> getSlotConfig
