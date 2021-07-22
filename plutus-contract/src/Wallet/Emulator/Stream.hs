{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
-- | Running emulator actions that produce streams of events
module Wallet.Emulator.Stream(
    -- * Emulator streams
    EmulatorConfig(..)
    , EmulatorErr(..)
    , InitialChainState
    , initialChainState
    , initialDist
    , initialState
    , slotConfig
    , feeConfig
    , runTraceStream
    -- * Stream manipulation
    , takeUntilSlot
    , filterLogLevel
    -- * Consuming streams
    , foldStreamM
    , foldEmulatorStreamM
    ) where

import qualified Control.Foldl                          as L
import           Control.Lens                           (filtered, makeLenses, preview, view)
import           Control.Monad.Freer                    (Eff, Member, interpret, reinterpret, run, subsume, type (~>))
import           Control.Monad.Freer.Coroutine          (Yield, yield)
import           Control.Monad.Freer.Error              (Error, runError)
import           Control.Monad.Freer.Extras             (raiseEnd, wrapError)
import           Control.Monad.Freer.Extras.Log         (LogLevel, LogMessage (..), LogMsg (..), logMessageContent,
                                                         mapMLog)
import           Control.Monad.Freer.Extras.Stream      (runStream)
import           Control.Monad.Freer.State              (State, gets, runState)
import           Data.Bifunctor                         (first)
import           Data.Default                           (Default (..))
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (fromMaybe)
import qualified Data.Set                               as Set
import qualified Ledger.AddressMap                      as AM
import           Ledger.Blockchain                      (Block, OnChainTx (..))
import           Ledger.Fee                             (FeeConfig)
import           Ledger.Slot                            (Slot)
import           Ledger.Value                           (Value)
import           Streaming                              (Stream)
import qualified Streaming                              as S
import           Streaming.Prelude                      (Of)
import qualified Streaming.Prelude                      as S
import           Wallet.API                             (WalletAPIError)
import           Wallet.Emulator                        (EmulatorEvent, EmulatorEvent')
import qualified Wallet.Emulator                        as EM
import           Wallet.Emulator.Chain                  (ChainControlEffect, ChainEffect, _SlotAdd)
import           Wallet.Emulator.MultiAgent             (EmulatorState, EmulatorTimeEvent (..), MultiAgentControlEffect,
                                                         MultiAgentEffect, chainEvent, eteEvent)
import           Wallet.Emulator.Wallet                 (Wallet (..), walletAddress)

-- TODO: Move these two to 'Wallet.Emulator.XXX'?
import           Ledger.TimeSlot                        (SlotConfig)
import           Plutus.Contract.Trace                  (InitialDistribution, defaultDist)
import           Plutus.Trace.Emulator.ContractInstance (EmulatorRuntimeError)

{- Note [Emulator event stream]

The primary way of observing the outcome of a trace is by looking at the
stream of events it produces, via 'runTraceStream'. This has the following
reasons:

* A totally ordered stream of events is a good way to characterise the
  behaviour of a dynamic system.
* By taking the stream of events as the main output of running a trace, we
  can potentially run the trace against a live system. (To really do that we'll have to change the type of log messages - 'EmulatorEvent' contains some events only make sense in the emulator. But the underlying mechanism of how the stream is produces is still the same.) See note [The Emulator Control effect]
* We have the potential of saving some work because the stream is produced
  on-demand. This also makes it possible to deal with infinite traces: We just
  evaluate them to a finite number of steps.

-}

-- | Finish the stream at the end of the given slot.
takeUntilSlot :: forall effs a. Slot -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ()
takeUntilSlot maxSlot = S.takeWhile (maybe True (\sl -> sl <= maxSlot) . preview (logMessageContent . eteEvent . chainEvent . _SlotAdd))

-- | Remove from the stream all log messages whose log level is lower than the
--   the given level.
filterLogLevel :: forall effs a. LogLevel -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
filterLogLevel lvl = S.mapMaybe (preview (filtered (\LogMessage{_logLevel} -> lvl <= _logLevel)))

-- | Apply a fold to an effectful stream of events.
foldStreamM :: forall m a b c.
    Monad m
    => L.FoldM m a b
    -> S.Stream (S.Of a) m c
    -> m (S.Of b c)
foldStreamM = L.impurely S.foldM

-- | Consume an emulator event stream.
foldEmulatorStreamM :: forall effs a b.
    L.FoldM (Eff effs) EmulatorEvent b
    -> S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) a
    -> Eff effs (S.Of b a)
foldEmulatorStreamM theFold =
    foldStreamM (L.premapM (pure . view logMessageContent) theFold)

-- | Turn an emulator action into a 'Stream' of emulator log messages, returning
--   the final state of the emulator.
runTraceStream :: forall effs.
    EmulatorConfig
    -> Eff '[ State EmulatorState
            , LogMsg EmulatorEvent'
            , MultiAgentEffect
            , MultiAgentControlEffect
            , ChainEffect
            , ChainControlEffect
            , Error EmulatorRuntimeError
            ] ()
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr, EmulatorState)
runTraceStream conf@EmulatorConfig{_slotConfig, _feeConfig} =
    fmap (first (either Just (const Nothing)))
    . S.hoist (pure . run)
    . runStream @(LogMessage EmulatorEvent) @_ @'[]
    . runState (initialState conf)
    . interpret handleLogCoroutine
    . reinterpret @_ @(LogMsg EmulatorEvent) (mkTimedLogs @EmulatorEvent')
    . runError
    . wrapError WalletErr
    . wrapError AssertionErr
    . wrapError InstanceErr
    . EM.processEmulated _slotConfig _feeConfig
    . subsume
    . subsume @(State EmulatorState)
    . raiseEnd

data EmulatorConfig =
    EmulatorConfig
        { _initialChainState :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        , _slotConfig        :: SlotConfig -- ^ Set the start time of slot 0 and the length of one slot
        , _feeConfig         :: FeeConfig -- ^ Configure the fee of a transaction
        } deriving (Eq, Show)

type InitialChainState = Either InitialDistribution EM.TxPool

-- | The wallets' initial funds
initialDist :: InitialChainState -> InitialDistribution
initialDist = either id (walletFunds . map Valid) where
    walletFunds :: Block -> Map Wallet Value
    walletFunds theBlock =
        let values = AM.values $ AM.fromChain [theBlock]
            getFunds wllt = fromMaybe mempty $ Map.lookup (walletAddress wllt) values
        in Map.fromSet getFunds (Set.fromList $ Wallet <$> [1..10])

instance Default EmulatorConfig where
  def = EmulatorConfig
          { _initialChainState = Left defaultDist
          , _slotConfig = def
          , _feeConfig = def
          }

initialState :: EmulatorConfig -> EM.EmulatorState
initialState EmulatorConfig{_initialChainState} =
    either
        (EM.emulatorStateInitialDist . Map.mapKeys EM.walletPubKey)
        EM.emulatorStatePool
        _initialChainState

data EmulatorErr =
    WalletErr WalletAPIError
    | AssertionErr EM.AssertionError
    | InstanceErr EmulatorRuntimeError
    deriving (Show)

handleLogCoroutine :: forall e effs.
    Member (Yield (LogMessage e) ()) effs
    => LogMsg e
    ~> Eff effs
handleLogCoroutine = \case LMessage m -> yield m id

-- | Annotate emulator log messages with the current system time
--   (slot number)
mkTimedLogs :: forall a effs.
    ( Member (LogMsg (EmulatorTimeEvent a)) effs
    , Member (State EmulatorState) effs
    )
    => LogMsg a
    ~> Eff effs
mkTimedLogs = mapMLog f where
    f :: a -> Eff effs (EmulatorTimeEvent a)
    f a =
        EmulatorTimeEvent
            <$> gets (view $ EM.chainState . EM.currentSlot)
            <*> pure a

makeLenses ''EmulatorConfig
