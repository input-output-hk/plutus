{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-

An emulator trace is a contract trace that can be run in the Plutus emulator.

-}
module Plutus.Trace.Emulator(
    Emulator
    , EmulatorTrace
    , EmulatorErr(..)
    , ContractHandle(..)
    , ContractInstanceTag
    , ContractConstraints
    -- * Constructing Traces
    , RunContract.activateContract
    , RunContract.activateContractWallet
    , RunContract.walletInstanceTag
    , RunContract.callEndpoint
    , RunContract.getContractState
    , RunContract.observableState
    , RunContract.activeEndpoints
    , EmulatedWalletAPI.liftWallet
    , EmulatedWalletAPI.payToWallet
    , Waiting.nextSlot
    , Waiting.waitUntilSlot
    , Waiting.waitUntilTime
    , Waiting.waitNSlots
    , Waiting.waitNMilliSeconds
    , EmulatorControl.freezeContractInstance
    , EmulatorControl.thawContractInstance
    -- ** Inspecting the chain state
    , EmulatorControl.setSigningProcess
    , EmulatorControl.chainState
    , ChainState.chainNewestFirst
    , ChainState.txPool
    , ChainState.index
    , ChainState.currentSlot
    -- ** Inspecting the agent states
    , EmulatorControl.agentState
    , Wallet.ownPrivateKey
    , Wallet.nodeClient
    , Wallet.chainIndex
    , Wallet.signingProcess
    -- * Throwing errors
    , throwError
    , EmulatorRuntimeError(..)
    -- * Running traces
    , EmulatorConfig(..)
    , initialChainState
    , slotConfig
    , feeConfig
    , runEmulatorStream
    , TraceConfig(..)
    , runEmulatorTrace
    , PrintEffect(..)
    , runEmulatorTraceEff
    , runEmulatorTraceIO
    , runEmulatorTraceIO'
    -- * Interpreter
    , interpretEmulatorTrace
    ) where

import           Control.Foldl                           (generalize, list)
import           Control.Lens                            hiding ((:>))
import           Control.Monad                           (forM_, void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine           (Yield)
import           Control.Monad.Freer.Error               (Error, handleError, throwError)
import           Control.Monad.Freer.Extras.Log          (LogMessage (..), LogMsg (..), mapLog)
import           Control.Monad.Freer.Extras.Modify       (raiseEnd)
import           Control.Monad.Freer.Reader              (Reader, runReader)
import           Control.Monad.Freer.State               (State, evalState)
import           Control.Monad.Freer.TH                  (makeEffect)
import           Data.Default                            (Default (..))
import qualified Data.Map                                as Map
import           Data.Maybe                              (fromMaybe)
import           Data.Text.Prettyprint.Doc               (defaultLayoutOptions, layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           Plutus.Trace.Scheduler                  (EmSystemCall, ThreadId, exit, runThreads)
import           System.IO                               (Handle, hPutStrLn, stdout)
import           Wallet.Emulator.Chain                   (ChainControlEffect)
import qualified Wallet.Emulator.Chain                   as ChainState
import           Wallet.Emulator.MultiAgent              (EmulatorEvent, EmulatorEvent' (..), EmulatorState (..),
                                                          MultiAgentControlEffect, MultiAgentEffect, _eteEmulatorTime,
                                                          _eteEvent, schedulerEvent)
import           Wallet.Emulator.Stream                  (EmulatorConfig (..), EmulatorErr (..), feeConfig,
                                                          foldEmulatorStreamM, initialChainState, initialDist,
                                                          runTraceStream, slotConfig)
import           Wallet.Emulator.Wallet                  (Entity, balances)
import qualified Wallet.Emulator.Wallet                  as Wallet

import           Plutus.Trace.Effects.ContractInstanceId (ContractInstanceIdEff, handleDeterministicIds)
import           Plutus.Trace.Effects.EmulatedWalletAPI  (EmulatedWalletAPI, handleEmulatedWalletAPI)
import qualified Plutus.Trace.Effects.EmulatedWalletAPI  as EmulatedWalletAPI
import           Plutus.Trace.Effects.EmulatorControl    (EmulatorControl, handleEmulatorControl)
import qualified Plutus.Trace.Effects.EmulatorControl    as EmulatorControl
import           Plutus.Trace.Effects.RunContract        (RunContract, handleRunContract)
import qualified Plutus.Trace.Effects.RunContract        as RunContract
import           Plutus.Trace.Effects.Waiting            (Waiting, handleWaiting)
import qualified Plutus.Trace.Effects.Waiting            as Waiting
import           Plutus.Trace.Emulator.System            (launchSystemThreads)
import           Plutus.Trace.Emulator.Types             (ContractConstraints, ContractHandle (..),
                                                          ContractInstanceLog (..), ContractInstanceMsg (..),
                                                          ContractInstanceTag, Emulator, EmulatorMessage (..),
                                                          EmulatorRuntimeError (..), EmulatorThreads,
                                                          UserThreadMsg (..))
import           Streaming                               (Stream)
import           Streaming.Prelude                       (Of (..))

import qualified Data.Aeson                              as A
import           Plutus.V1.Ledger.Slot                   (getSlot)
import           Plutus.V1.Ledger.Value                  (Value (..), flattenValue)

-- | A very simple effect for interpreting the output printing done by the
-- trace printing functions:
--
-- * 'runEmulatorTraceEff'
-- * 'runEmulatorTraceIO'
-- * 'runEmulatorTraceIO''
data PrintEffect r where
  PrintLn :: String -> PrintEffect ()
makeEffect ''PrintEffect

type EmulatorTrace a =
        Eff
            '[ RunContract
            , Waiting
            , EmulatorControl
            , EmulatedWalletAPI
            , LogMsg String
            , Error EmulatorRuntimeError
            ] a

handleEmulatorTrace ::
    forall effs a.
    ( Member MultiAgentEffect effs
    , Member MultiAgentControlEffect effs
    , Member (State EmulatorThreads) effs
    , Member (State EmulatorState) effs
    , Member (Error EmulatorRuntimeError) effs
    , Member (LogMsg EmulatorEvent') effs
    , Member ContractInstanceIdEff effs
    )
    => EmulatorTrace a
    -> Eff (Reader ThreadId ': Yield (EmSystemCall effs EmulatorMessage) (Maybe EmulatorMessage) ': effs) ()
handleEmulatorTrace action = do
    _ <- subsume @(Error EmulatorRuntimeError)
            . interpret (mapLog (UserThreadEvent . UserLog))
            . flip handleError (throwError . EmulatedWalletError)
            . reinterpret handleEmulatedWalletAPI
            . interpret (handleEmulatorControl @_ @effs)
            . interpret (handleWaiting @_ @effs)
            . interpret (handleRunContract @_ @effs)
            $ raiseEnd action
    void $ exit @effs @EmulatorMessage

-- | Run a 'Trace Emulator', streaming the log messages as they arrive
runEmulatorStream :: forall effs a.
    EmulatorConfig
    -> EmulatorTrace a
    -> Stream (Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr, EmulatorState)
runEmulatorStream conf = runTraceStream conf . interpretEmulatorTrace conf

-- | Interpret a 'Trace Emulator' action in the multi agent and emulated
--   blockchain effects.
interpretEmulatorTrace :: forall effs a.
    ( Member MultiAgentEffect effs
    , Member MultiAgentControlEffect effs
    , Member (Error EmulatorRuntimeError) effs
    , Member ChainControlEffect effs
    , Member (LogMsg EmulatorEvent') effs
    , Member (State EmulatorState) effs
    )
    => EmulatorConfig
    -> EmulatorTrace a
    -> Eff effs ()
interpretEmulatorTrace conf action =
    -- add a wait action to the beginning to ensure that the
    -- initial transaction gets validated before the wallets
    -- try to spend their funds
    let action' = Waiting.nextSlot >> action >> Waiting.nextSlot
        defaultWallets = Wallet.Wallet <$> [1..10]
        wallets = fromMaybe defaultWallets (preview (initialChainState . _Left . to Map.keys) conf)
    in
    evalState @EmulatorThreads mempty
        $ handleDeterministicIds
        $ interpret (mapLog (review schedulerEvent))
        $ runThreads
        $ do
            raise $ launchSystemThreads wallets
            handleEmulatorTrace action'

-- | Options for how to set up and print the trace.
data TraceConfig = TraceConfig
  { showEvent    :: EmulatorEvent' -> Maybe String
  -- ^ Function to decide how to print the particular events.
  , outputHandle :: Handle
  -- ^ Where to print the outputs to. Default: 'System.IO.stdout'
  }

instance Default TraceConfig where
  def = TraceConfig
            { showEvent     = defaultShowEvent
            , outputHandle  = stdout
            }

defaultShowEvent :: EmulatorEvent' -> Maybe String
defaultShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
  InstanceEvent (ContractInstanceLog NoRequestsHandled            _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _)           _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _)          _ _) -> Nothing
  SchedulerEvent _                                                     -> Nothing
  ChainIndexEvent _ _                                                  -> Nothing
  WalletEvent _ _                                                      -> Nothing
  ev                                                                   -> Just . renderString . layoutPretty defaultLayoutOptions . pretty $ ev

-- | Run an emulator trace to completion, returning a tuple of the final state
-- of the emulator, the events, and any error, if any.
runEmulatorTrace
    :: EmulatorConfig
    -> EmulatorTrace ()
    -> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState)
runEmulatorTrace cfg trace =
    (\(xs :> (y, z)) -> (xs, y, z))
    $ run
    $ runReader ((initialDist . _initialChainState) cfg)
    $ foldEmulatorStreamM (generalize list)
    $ runEmulatorStream cfg trace


-- | Run the emulator trace returning an effect that can be evaluated by
-- interpreting the 'PrintEffect's.
runEmulatorTraceEff :: forall effs. Member PrintEffect effs
    => TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace ()
    -> Eff effs ()
runEmulatorTraceEff tcfg cfg trace =
  let (xs, me, e) = runEmulatorTrace cfg trace
      balances' = balances (_chainState e) (_walletStates e)
   in do
      case me of
        Nothing  -> return ()
        Just err -> printLn $ "ERROR: " <> show err

      forM_ xs $ \ete -> do
        case showEvent tcfg (_eteEvent ete) of
          Nothing -> return ()
          Just s  ->
            let slot = pad 5 (getSlot $ _eteEmulatorTime ete)
             in printLn $ "Slot " <> slot <> ": " <> s

      printLn "Final balances"
      printBalances balances'

-- | Runs the trace with 'runEmulatorTrace', with default configuration that
-- prints a selection of events to stdout.
--
-- Example:
--
-- >>> runEmulatorTraceIO (void $ Trace.waitNSlots 1)
runEmulatorTraceIO
    :: EmulatorTrace ()
    -> IO ()
runEmulatorTraceIO = runEmulatorTraceIO' def def

--- | Runs the trace with a given configuration for the trace and the config.
--
-- Example of running a trace and saving the output to a file:
--
-- >>> withFile "/tmp/trace-log.txt" WriteMode $ \h -> runEmulatorTraceIO' (def { outputHandle = h }) def (void $ Trace.waitNSlots 1)
runEmulatorTraceIO'
    :: TraceConfig
    -> EmulatorConfig
    -> EmulatorTrace ()
    -> IO ()
runEmulatorTraceIO' tcfg cfg trace
  = runPrintEffect (outputHandle tcfg) $ runEmulatorTraceEff tcfg cfg trace

runPrintEffect :: Handle
         -> Eff '[PrintEffect, IO] r
         -> IO r
runPrintEffect hdl = runM . interpretM f
  where
    f :: PrintEffect r -> IO r
    f = \case
      PrintLn s -> hPutStrLn hdl s

pad :: Int -> Integer -> String
pad n = (\x -> replicate (n - length x) '0' ++ x) . show

printBalances :: forall effs. Member PrintEffect effs
              => Map.Map Entity Value
              -> Eff effs ()
printBalances m = do
    forM_ (Map.toList m) $ \(e, v) -> do
        printLn $ show e <> ": "
        forM_ (flattenValue v) $ \(cs, tn, a) ->
            printLn $ "    {" <> show cs <> ", " <> show tn <> "}: " <> show a
