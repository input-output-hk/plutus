{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Chain where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens                   hiding (index)
import           Control.Monad.Freer
import           Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import           Control.Monad.Freer.State      (State, get, gets, modify, put)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Foldable                  (traverse_)
import           Data.Functor                   (void)
import           Data.Maybe                     (listToMaybe)
import           GHC.Generics                   (Generic)
import           Ledger                         (Block, Slot (..), Tx (..))
import qualified Ledger.Index                   as Index
import           Ledger.TimeSlot                (SlotConfig)
import qualified Wallet.Emulator.Chain          as EC

type TxPool = [Tx]

data MockNodeServerChainState = MockNodeServerChainState
  { _txPool      :: TxPool
  , _index       :: Index.UtxoIndex
  , _currentSlot :: Slot
  , _channel     :: TChan Block
  , _tip         :: Maybe Block
  } deriving (Generic)

makeLenses ''MockNodeServerChainState

instance Show MockNodeServerChainState where
    -- Skip showing the full chain
    show MockNodeServerChainState {_txPool, _index, _currentSlot, _tip} =
        "MockNodeServerChainState { " <> show _txPool
                        <> ", " <> show _index
                        <> ", " <> show _currentSlot
                        <> ", " <> show _tip <> " }"

emptyChainState :: MonadIO m => m MockNodeServerChainState
emptyChainState = do
    chan <- liftIO . atomically $ newTChan
    pure $ MockNodeServerChainState [] mempty 0 chan Nothing

getChannel :: MonadIO m => MVar MockNodeServerChainState -> m (TChan Block)
getChannel mv = liftIO (readMVar mv) <&> view channel

-- | Build a PAB ChainState from a emulator ChainState
fromEmulatorChainState :: MonadIO m => EC.ChainState -> m MockNodeServerChainState
fromEmulatorChainState EC.ChainState {EC._txPool, EC._index, EC._currentSlot, EC._chainNewestFirst} = do
    ch <- liftIO $ atomically newTChan
    void $ liftIO $
        mapM_ (atomically . writeTChan ch) _chainNewestFirst
    pure $ MockNodeServerChainState { _channel     = ch
                      , _txPool      = _txPool
                      , _index       = _index
                      , _currentSlot = _currentSlot
                      , _tip         = listToMaybe _chainNewestFirst
                      }

-- Get the current tip or wait for one if there are no blocks.
getTip :: forall m. MonadIO m => MVar MockNodeServerChainState -> m Block
getTip mvChainState = liftIO $ readMVar mvChainState >>= \case
    MockNodeServerChainState { _tip = Just tip' } -> pure tip'
    MockNodeServerChainState { _channel }         -> do
        -- Wait for the initial block.
        void $ liftIO $ atomically $ peekTChan _channel
        getTip mvChainState

handleControlChain ::
     ( Member (State MockNodeServerChainState) effs
     , Member (LogMsg EC.ChainEvent) effs
     , LastMember m effs
     , MonadIO m )
  => EC.ChainControlEffect ~> Eff effs
handleControlChain = \case
    EC.ProcessBlock -> do
        st <- get
        let pool  = st ^. txPool
            slot  = st ^. currentSlot
            idx   = st ^. index
            EC.ValidatedBlock block events rest =
                EC.validateBlock slot idx pool

        let st' = st & txPool .~ rest
                     & tip    ?~ block
                     & index  %~ Index.insertBlock block

        put st'
        traverse_ logEvent events

        liftIO $ atomically $ writeTChan (st ^. channel) block
        pure block
    EC.ModifySlot f -> modify @MockNodeServerChainState (over currentSlot f) >> gets (view currentSlot)

handleChain ::
     ( Member (State MockNodeServerChainState) effs )
  => SlotConfig
  -> EC.ChainEffect ~> Eff effs
handleChain slotCfg = \case
    EC.QueueTx tx     -> modify $ over txPool (addTxToPool tx)
    EC.GetCurrentSlot -> gets _currentSlot
    EC.GetSlotConfig  -> pure slotCfg

logEvent :: Member (LogMsg EC.ChainEvent) effs => EC.ChainEvent -> Eff effs ()
logEvent e = case e of
    EC.SlotAdd{}           -> logDebug e
    EC.TxnValidationFail{} -> logWarn e
    _                      -> logInfo e

addTxToPool :: Tx -> TxPool -> TxPool
addTxToPool = (:)

-- | Fetch the currently stored chain by iterating over the channel until
--   there is nothing left to be returned.
chainNewestFirst :: forall m. MonadIO m => TChan Block -> m [Block]
chainNewestFirst ch = do
    localChannel <- liftIO $ atomically $ cloneTChan ch
    go localChannel []
    where
    go :: TChan Block -> [Block] -> m [Block]
    go local acc =
        (liftIO $ atomically $ tryReadTChan local) >>= \case
            Nothing    -> pure acc
            Just block -> go ch (block : acc)
