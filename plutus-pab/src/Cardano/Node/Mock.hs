{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Node.Mock where

import           Control.Concurrent                  (threadDelay)
import           Control.Concurrent.MVar             (MVar, modifyMVar_, putMVar, takeMVar)
import           Control.Lens                        (over, set, unto, view)
import           Control.Monad                       (forever, unless, void)
import           Control.Monad.Freer                 (Eff, LastMember, Member, interpret, reinterpret, runM, subsume)
import           Control.Monad.Freer.Extras.Log
import           Control.Monad.Freer.Extras.Modify   (handleZoomedState)
import           Control.Monad.Freer.Reader          (Reader)
import qualified Control.Monad.Freer.Reader          as Eff
import qualified Control.Monad.Freer.State           as Eff
import qualified Control.Monad.Freer.Writer          as Eff
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Foldable                       (traverse_)
import           Data.Function                       ((&))
import           Data.Time.Units                     (Millisecond, Second, toMicroseconds)
import           Data.Time.Units.Extra               ()
import           Servant                             (NoContent (NoContent))

import           Cardano.BM.Data.Trace               (Trace)
import           Cardano.Chain                       (handleChain, handleControlChain)
import           Cardano.Node.RandomTx
import           Cardano.Node.Types
import qualified Cardano.Protocol.Socket.Mock.Client as Client
import qualified Cardano.Protocol.Socket.Mock.Server as Server
import           Ledger                              (Tx)
import           Ledger.TimeSlot                     (SlotConfig (SlotConfig, scSlotLength), currentSlot)
import           Ledger.Tx                           (outputs)
import           Plutus.PAB.Arbitrary                ()
import qualified Plutus.PAB.Monitoring.Monitoring    as LM
import qualified Wallet.Emulator.Chain               as Chain

healthcheck :: Monad m => m NoContent
healthcheck = pure NoContent

consumeEventHistory :: MonadIO m => MVar AppState -> m [LogMessage MockServerLogMsg]
consumeEventHistory stateVar =
    liftIO $ do
        oldState <- takeMVar stateVar
        let events = view eventHistory oldState
        let newState = set eventHistory mempty oldState
        putMVar stateVar newState
        pure events

addTx ::
    ( Member (LogMsg MockServerLogMsg) effs
    , Member (Reader Client.TxSendHandle) effs
    , MonadIO m
    , LastMember m effs
    )
 => Tx -> Eff effs NoContent
addTx tx = do
    logInfo $ BlockOperation $ NewTransaction tx
    clientHandler <- Eff.ask
    liftIO $ Client.queueTx clientHandler tx
    pure NoContent

-- | Run all chain effects in the IO Monad
runChainEffects ::
 Trace IO MockServerLogMsg
 -> SlotConfig
 -> Client.TxSendHandle
 -> MVar AppState
 -> Eff (NodeServerEffects IO) a
 -> IO ([LogMessage MockServerLogMsg], a)
runChainEffects trace slotCfg clientHandler stateVar eff = do
    oldAppState <- liftIO $ takeMVar stateVar
    ((a, events), newState) <- liftIO
            $ processBlock eff
            & runRandomTx
            & runChain
            & mergeState
            & toWriter
            & runReaders oldAppState
            & interpret (LM.handleLogMsgTrace trace)
            & runM
    liftIO $ putMVar stateVar newState
    pure (events, a)
        where
            processBlock e = e >>= \r -> Chain.processBlock >> pure r

            runRandomTx = subsume . runGenRandomTx

            runChain = interpret (mapLog ProcessingChainEvent)
                     . reinterpret (handleChain slotCfg)
                     . interpret (mapLog ProcessingChainEvent)
                     . reinterpret handleControlChain

            mergeState = interpret (handleZoomedState chainState)

            toWriter = Eff.runWriter . reinterpret (handleLogWriter @MockServerLogMsg @[LogMessage MockServerLogMsg] (unto return))

            runReaders s = Eff.runState s . Eff.runReader clientHandler

processChainEffects ::
    Trace IO MockServerLogMsg
    -> SlotConfig
    -> Client.TxSendHandle
    -> MVar AppState
    -> Eff (NodeServerEffects IO) a
    -> IO a
processChainEffects trace slotCfg clientHandler stateVar eff = do
    (events, result) <- liftIO $ runChainEffects trace slotCfg clientHandler stateVar eff
    LM.runLogEffects trace $ traverse_ (\(LogMessage _ chainEvent) -> logDebug chainEvent) events
    liftIO $
        modifyMVar_
            stateVar
            (\state -> pure $ over eventHistory (mappend events) state)
    pure result

-- | Generates a random transaction once in each 'mscRandomTxInterval' of the
--   config
transactionGenerator ::
  Trace IO MockServerLogMsg
  -> SlotConfig
  -> Second
  -> Client.TxSendHandle
  -> MVar AppState
  -> IO ()
transactionGenerator trace slotCfg interval clientHandler stateVar =
    forever $ do
        liftIO $ threadDelay $ fromIntegral $ toMicroseconds interval
        processChainEffects trace slotCfg clientHandler stateVar $ do
            tx' <- genRandomTx
            unless (null $ view outputs tx') (void $ addTx tx')

-- | Calls 'addBlock' at the start of every slot, causing pending transactions
--   to be validated and added to the chain.
slotCoordinator ::
    SlotConfig
    -> Server.ServerHandler
    -> IO a
slotCoordinator sc@SlotConfig{scSlotLength} serverHandler = do
    forever $ do
        void $ Server.processBlock serverHandler
        newSlot <- currentSlot sc
        void $ Server.modifySlot (const newSlot) serverHandler
        liftIO $ threadDelay
               $ fromIntegral
               $ toMicroseconds (fromInteger scSlotLength :: Millisecond)
