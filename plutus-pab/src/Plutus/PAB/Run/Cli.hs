{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}

module Plutus.PAB.Run.Cli (ConfigCommandArgs(..), runConfigCommand, runNoConfigCommand) where

-----------------------------------------------------------------------------------------------------------------------
-- Command interpretation
-----------------------------------------------------------------------------------------------------------------------

import           Cardano.BM.Configuration              (Configuration)
import qualified Cardano.BM.Configuration.Model        as CM
import           Cardano.BM.Data.Trace                 (Trace)
import qualified Cardano.ChainIndex.Server             as ChainIndex
import qualified Cardano.Metadata.Server               as Metadata
import qualified Cardano.Node.Server                   as NodeServer
import           Cardano.Node.Types                    (MockServerConfig (..))
import qualified Cardano.Wallet.Server                 as WalletServer
import           Cardano.Wallet.Types
import           Control.Concurrent                    (takeMVar)
import           Control.Concurrent.Async              (Async, async, waitAny)
import           Control.Concurrent.Availability       (Availability, starting)
import qualified Control.Concurrent.STM                as STM
import           Control.Monad                         (forM, forM_, void)
import           Control.Monad.Freer                   (Eff, LastMember, Member, interpret, runM)
import           Control.Monad.Freer.Delay             (DelayEffect, delayThread, handleDelayEffect)
import           Control.Monad.Freer.Error             (throwError)
import           Control.Monad.Freer.Extras.Log        (logInfo)
import           Control.Monad.Freer.Reader            (ask, runReader)
import           Control.Monad.IO.Class                (liftIO)
import           Data.Aeson                            (FromJSON, ToJSON)
import           Data.Foldable                         (traverse_)
import qualified Data.Map                              as Map
import           Data.Proxy                            (Proxy (..))
import qualified Data.Set                              as Set
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (Pretty (..), defaultLayoutOptions, layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Time.Units                       (Second)
import           Data.Typeable                         (Typeable)
import           Plutus.Contract.Resumable             (responses)
import           Plutus.Contract.State                 (State (..))
import qualified Plutus.Contract.State                 as State
import qualified Plutus.PAB.App                        as App
import qualified Plutus.PAB.Core                       as Core
import           Plutus.PAB.Core.ContractInstance      (ContractInstanceState (..), updateState)
import           Plutus.PAB.Core.ContractInstance.STM  (InstanceState, emptyInstanceState)
import qualified Plutus.PAB.Db.Beam                    as Beam
import qualified Plutus.PAB.Effects.Contract           as Contract
import           Plutus.PAB.Effects.Contract.Builtin   (Builtin, BuiltinHandler, HasDefinitions (..),
                                                        SomeBuiltinState (..), getResponse)
import qualified Plutus.PAB.Monitoring.Monitoring      as LM
import           Plutus.PAB.Run.Command
import           Plutus.PAB.Run.PSGenerator            (HasPSTypes (..))
import qualified Plutus.PAB.Run.PSGenerator            as PSGenerator
import           Plutus.PAB.Types                      (Config (..), DbConfig (..), chainIndexConfig,
                                                        metadataServerConfig, nodeServerConfig, walletServerConfig)
import qualified Plutus.PAB.Webserver.Server           as PABServer
import           Plutus.PAB.Webserver.Types            (ContractActivationArgs (..))
import qualified Servant
import qualified Wallet.Types                          as Wallet

runNoConfigCommand ::
    Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
    -> NoConfigCommand
    -> IO ()
runNoConfigCommand trace = \case

    -- Run database migration
    Migrate{dbPath} ->
        let conf = DbConfig{dbConfigPoolSize=10, dbConfigFile=Text.pack dbPath} in
        App.migrate (LM.convertLog LM.PABMsg trace) conf

    -- Generate PureScript bridge code
    PSGenerator {psGenOutputDir} -> do
        PSGenerator.generateDefault psGenOutputDir

    -- Get default logging configuration
    WriteDefaultConfig{outputFile} -> LM.defaultConfig >>= flip CM.exportConfiguration outputFile

data ConfigCommandArgs a =
    ConfigCommandArgs
        { ccaTrace          :: Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
        , ccaLoggingConfig  :: Configuration -- ^ Monitoring configuration
        , ccaPABConfig      :: Config        -- ^ PAB Configuration
        , ccaAvailability   :: Availability  -- ^ Token for signaling service availability
        , ccaStorageBackend :: App.StorageBackend -- ^ Wheter to use the beam-sqlite or in-memory backend
        }

-- | Interpret a 'Command' in 'Eff' using the provided tracer and configurations
--
runConfigCommand :: forall a.
    ( Ord a
    , Show a
    , ToJSON a
    , FromJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , Typeable a
    , HasDefinitions a
    , HasPSTypes a
    )
    => BuiltinHandler a
    -> ConfigCommandArgs a
    -> ConfigCommand
    -> IO ()

-- Run mock wallet service
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig, chainIndexConfig, walletServerConfig},ccaAvailability} MockWallet =
    liftIO $ WalletServer.main
        (toWalletLog ccaTrace)
        walletServerConfig
        (mscFeeConfig nodeServerConfig)
        (mscSocketPath nodeServerConfig)
        (mscSlotConfig nodeServerConfig)
        (ChainIndex.ciBaseUrl chainIndexConfig)
        ccaAvailability

-- Run mock node server
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig},ccaAvailability} (MockNode withoutMockServer) =
    liftIO $ NodeServer.main
        (toMockNodeServerLog ccaTrace)
        nodeServerConfig
        withoutMockServer
        ccaAvailability

-- Run mock metadata server
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {metadataServerConfig}, ccaAvailability} Metadata =
    liftIO $ Metadata.main
        (toMetaDataLog ccaTrace)
        metadataServerConfig
        ccaAvailability

-- Run PAB webserver
runConfigCommand contractHandler ConfigCommandArgs{ccaTrace, ccaPABConfig=config@Config{pabWebserverConfig, dbConfig}, ccaAvailability, ccaStorageBackend} PABWebserver =
    do
        connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
        -- Restore the running contracts by first collecting up enough details about the
        -- previous contracts to re-start them
        previousContracts <-
            Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
            $ interpret (LM.handleLogMsgTrace ccaTrace)
            $ do
                cIds   <- Map.toList <$> Contract.getActiveContracts @(Builtin a)
                forM cIds $ \(cid, args) -> do
                    s <- Contract.getState @(Builtin a) cid
                    let priorContract :: (SomeBuiltinState a, Wallet.ContractInstanceId, ContractActivationArgs a)
                        priorContract = (s, cid, args)
                    pure priorContract

        -- Then, start the server
        fmap (either (error . show) id)
          $ App.runApp ccaStorageBackend (toPABMsg ccaTrace) contractHandler config
          $ do
              env <- ask @(Core.PABEnvironment (Builtin a) (App.AppEnv a))
              -- But first, spin up all the previous contracts
              case previousContracts of
                -- TODO: Log this error a bit better? Or handle it earlier?
                Left err -> throwError err
                Right ts -> do
                    forM_ ts $ \(s, cid, args) -> do
                      action <- buildPABAction @a @(App.AppEnv a) s cid args
                      liftIO $ Core.runPAB' env action
                      pure ()

              -- then, actually start the server.
              let walletClientEnv = App.walletClientEnv (Core.appEnv env)
              (mvar, _) <- PABServer.startServer pabWebserverConfig (Left walletClientEnv) ccaAvailability
              liftIO $ takeMVar mvar

-- Fork a list of commands
runConfigCommand contractHandler c@ConfigCommandArgs{ccaAvailability} (ForkCommands commands) =
    void $ do
        threads <- traverse forkCommand commands
        putStrLn "Started all commands."
        waitAny threads
  where
    forkCommand :: ConfigCommand -> IO (Async ())
    forkCommand subcommand = do
      putStrLn $ "Starting: " <> show subcommand
      asyncId <- async . void . runConfigCommand contractHandler c $ subcommand
      putStrLn $ "Started: " <> show subcommand
      starting ccaAvailability
      pure asyncId

-- Run the chain-index service
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config {nodeServerConfig, chainIndexConfig}, ccaAvailability} ChainIndex =
    ChainIndex.main
        (toChainIndexLog ccaTrace)
        chainIndexConfig
        (mscSocketPath nodeServerConfig)
        (mscSlotConfig nodeServerConfig)
        ccaAvailability

-- Get the state of a contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} (ContractState contractInstanceId) = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            s <- Contract.getState @(Builtin a) contractInstanceId
            let outputState = Contract.serialisableState (Proxy @(Builtin a)) s
            logInfo @(LM.AppMsg (Builtin a)) $ LM.PABMsg $ LM.SCoreMsg $ LM.FoundContract $ Just outputState
            drainLog

-- Get all available contracts
runConfigCommand _ ConfigCommandArgs{ccaTrace} ReportAvailableContracts = do
    runM
        $ interpret (App.handleContractDefinition @a)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ handleDelayEffect
        $ do
            availableContracts <- Contract.getDefinitions @(Builtin a)
            traverse_ (logInfo @(LM.AppMsg (Builtin a)) . LM.AvailableContract . render . pretty) availableContracts
            drainLog
                where
                    render = renderStrict . layoutPretty defaultLayoutOptions

-- Get all active contracts
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} ReportActiveContracts = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            logInfo @(LM.AppMsg (Builtin a)) LM.ActiveContractsMsg
            instancesById <- Contract.getActiveContracts @(Builtin a)
            let idsByDefinition = Map.fromListWith (<>) $ fmap (\(inst, ContractActivationArgs{caID}) -> (caID, Set.singleton inst)) $ Map.toList instancesById
            traverse_ (\(e, s) -> logInfo @(LM.AppMsg (Builtin a)) $ LM.ContractInstances e (Set.toList s)) $ Map.toAscList idsByDefinition
            drainLog

-- Get history of a specific contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} (ReportContractHistory contractInstanceId) = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            logInfo @(LM.AppMsg (Builtin a)) LM.ContractHistoryMsg
            s <- Contract.getState @(Builtin a) contractInstanceId
            let State.ContractResponse{State.newState=State{record}} = Contract.serialisableState (Proxy @(Builtin a)) s
            traverse_ logStep (responses record)
            drainLog
                where
                    logStep response = logInfo @(LM.AppMsg (Builtin a)) $ LM.ContractHistoryItem contractInstanceId (snd <$> response)

runConfigCommand _ _ PSApiGenerator {psApiGenOutputDir} = do
    PSGenerator.generateAPIModule (Proxy @a) psApiGenOutputDir
    PSGenerator.generateWith (Proxy @a) psApiGenOutputDir

toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
toPABMsg = LM.convertLog LM.PABMsg

toChainIndexLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.ChainIndexServerMsg
toChainIndexLog = LM.convertLog $ LM.PABMsg . LM.SChainIndexServerMsg

toWalletLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m WalletMsg
toWalletLog = LM.convertLog $ LM.PABMsg . LM.SWalletMsg

toMetaDataLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.MetadataLogMessage
toMetaDataLog = LM.convertLog $ LM.PABMsg . LM.SMetaDataLogMsg

toMockNodeServerLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.MockServerLogMsg
toMockNodeServerLog = LM.convertLog $ LM.PABMsg . LM.SMockserverLogMsg

-- | Wait for some time to allow all log messages to be printed to
--   the terminal.
drainLog :: Member DelayEffect effs => Eff effs ()
drainLog = delayThread (1 :: Second)

-- | Build a PAB Action that will run the provided context with the
-- reconstructed state.
buildPABAction ::
    forall a env effs.
    ( LastMember IO effs
    )
    => SomeBuiltinState a
    -> Wallet.ContractInstanceId
    -> ContractActivationArgs a
    -> Eff effs (Core.PABAction (Builtin a) env Wallet.ContractInstanceId)
buildPABAction currentState cid ContractActivationArgs{caWallet, caID} = do
    let r = getResponse currentState

    -- Bring up the STM state
    stmState :: InstanceState <- liftIO $ STM.atomically emptyInstanceState
    void $ runReader stmState $ updateState @IO r

    -- Squish it into a PAB action which we will run
    let action = Core.activateContract' @(Builtin a) (ContractInstanceState currentState (pure stmState)) cid caWallet caID

    pure action
