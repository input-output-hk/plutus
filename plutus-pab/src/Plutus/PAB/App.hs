{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Plutus.PAB.App(
    App,
    runApp,
    AppEnv(..),
    StorageBackend(..),
    -- * App actions
    migrate,
    dbConnect,
    ) where

import           Cardano.BM.Trace                               (Trace, logDebug)
import           Cardano.ChainIndex.Client                      (handleChainIndexClient)
import qualified Cardano.ChainIndex.Types                       as ChainIndex
import           Cardano.Node.Client                            (handleNodeClientClient)
import           Cardano.Node.Types                             (MockServerConfig (..))
import qualified Cardano.Protocol.Socket.Client                 as Client
import qualified Cardano.Protocol.Socket.Mock.Client            as MockClient
import qualified Cardano.Wallet.Client                          as WalletClient
import qualified Cardano.Wallet.Types                           as Wallet
import qualified Control.Concurrent.STM                         as STM
import           Control.Monad.Freer
import           Control.Monad.Freer.Error                      (handleError, throwError)
import           Control.Monad.Freer.Extras.Log                 (mapLog)
import           Control.Monad.IO.Class                         (MonadIO (..))
import           Data.Coerce                                    (coerce)
import           Data.Text                                      (Text, pack, unpack)
import           Database.Beam.Migrate.Simple
import qualified Database.Beam.Sqlite                           as Sqlite
import qualified Database.Beam.Sqlite.Migrate                   as Sqlite
import           Database.SQLite.Simple                         (open)
import qualified Database.SQLite.Simple                         as Sqlite
import           Ledger                                         (Block)
import           Network.HTTP.Client                            (managerModifyRequest, newManager,
                                                                 setRequestIgnoreStatus)
import           Network.HTTP.Client.TLS                        (tlsManagerSettings)
import           Plutus.PAB.Core                                (EffectHandlers (..), PABAction)
import qualified Plutus.PAB.Core                                as Core
import qualified Plutus.PAB.Core.ContractInstance.BlockchainEnv as BlockchainEnv
import           Plutus.PAB.Core.ContractInstance.STM           as Instances
import qualified Plutus.PAB.Db.Beam.ContractDefinitionStore     as BeamEff
import qualified Plutus.PAB.Db.Beam.ContractStore               as BeamEff
import           Plutus.PAB.Db.Memory.ContractStore             (InMemInstances, initialInMemInstances)
import qualified Plutus.PAB.Db.Memory.ContractStore             as InMem
import           Plutus.PAB.Effects.Contract.ContractExe        (ContractExe (..), handleContractEffectContractExe)
import           Plutus.PAB.Effects.DbStore                     (checkedSqliteDb, handleDbStore)
import           Plutus.PAB.Monitoring.Monitoring               (handleLogMsgTrace)
import           Plutus.PAB.Monitoring.PABLogMsg                (PABLogMsg (..), PABMultiAgentMsg (UserLog))
import           Plutus.PAB.Timeout                             (Timeout (..))
import           Plutus.PAB.Types                               (Config (Config), DbConfig (..), PABError (..),
                                                                 chainIndexConfig, dbConfig, endpointTimeout,
                                                                 nodeServerConfig, walletServerConfig)
import           Servant.Client                                 (ClientEnv, mkClientEnv)

------------------------------------------------------------

data AppEnv =
    AppEnv
        { dbConnection          :: Sqlite.Connection
        , walletClientEnv       :: ClientEnv
        , nodeClientEnv         :: ClientEnv
        , chainIndexEnv         :: ClientEnv
        , txSendHandle          :: MockClient.TxSendHandle
        , chainSyncHandle       :: Client.ChainSyncHandle Block
        , appConfig             :: Config
        , appTrace              :: Trace IO (PABLogMsg ContractExe)
        , appInMemContractStore :: InMemInstances ContractExe
        }

appEffectHandlers :: StorageBackend -> Config -> Trace IO (PABLogMsg ContractExe) -> EffectHandlers ContractExe AppEnv
appEffectHandlers storageBackend config trace =
    EffectHandlers
        { initialiseEnvironment = do
            env <- liftIO $ mkEnv trace config
            let Config{nodeServerConfig=MockServerConfig{mscSocketPath, mscSlotConfig}} = config
            instancesState <- liftIO $ STM.atomically $ Instances.emptyInstancesState
            blockchainEnv <- liftIO $ BlockchainEnv.startNodeClient mscSocketPath mscSlotConfig
            pure (instancesState, blockchainEnv, env)

        , handleLogMessages =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog SMultiAgent)

        , handleContractEffect =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog @_ @(PABLogMsg ContractExe) SContractExeLogMsg)
            . reinterpret (handleContractEffectContractExe @IO)

        , handleContractStoreEffect =
          case storageBackend of
            InMemoryBackend ->
              interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
              . interpret (Core.handleMappedReader @AppEnv appInMemContractStore)
              . reinterpret2 InMem.handleContractStore

            BeamSqliteBackend ->
              interpret (handleLogMsgTrace trace)
              . reinterpret (mapLog @_ @(PABLogMsg ContractExe) SMultiAgent)
              . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
              . interpret (Core.handleMappedReader @AppEnv dbConnection)
              . interpret (handleDbStore trace)
              . reinterpretN @'[_, _, _, _] BeamEff.handleContractStore

        , handleContractDefinitionStoreEffect =
            interpret (handleLogMsgTrace trace)
            . reinterpret (mapLog @_ @(PABLogMsg ContractExe) SMultiAgent)
           .  interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . interpret (Core.handleMappedReader @AppEnv dbConnection)
            . interpret (handleDbStore trace)
            . reinterpretN @'[_, _, _, _] BeamEff.handleContractDefinitionStore

        , handleServicesEffects = \wallet ->
            -- handle 'NodeClientEffect'
            flip handleError (throwError . NodeClientError)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @(Client.ChainSyncHandle Block) chainSyncHandle)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @MockClient.TxSendHandle txSendHandle)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @ClientEnv nodeClientEnv)
            . reinterpretN @'[_, _, _, _] (handleNodeClientClient @IO $ mscSlotConfig $ nodeServerConfig config)

            -- handle 'ChainIndexEffect'
            . flip handleError (throwError . ChainIndexError)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @ClientEnv chainIndexEnv)
            . reinterpret2 (handleChainIndexClient @IO)

            -- handle 'WalletEffect'
            . flip handleError (throwError . WalletClientError)
            . flip handleError (throwError . WalletError)
            . interpret (Core.handleUserEnvReader @ContractExe @AppEnv)
            . reinterpret (Core.handleMappedReader @AppEnv @ClientEnv walletClientEnv)
            . reinterpretN @'[_, _, _] (WalletClient.handleWalletClient @IO wallet)

        , onStartup = pure ()

        , onShutdown = pure ()
        }

runApp ::
    forall a.
    StorageBackend
    -> Trace IO (PABLogMsg ContractExe) -- ^ Top-level tracer
    -> Config -- ^ Client configuration
    -> App a -- ^ Action
    -> IO (Either PABError a)
runApp storageBackend trace config@Config{endpointTimeout} = Core.runPAB (Timeout endpointTimeout) (appEffectHandlers storageBackend config trace)

type App a = PABAction ContractExe AppEnv a

data StorageBackend = BeamSqliteBackend | InMemoryBackend
  deriving (Eq, Ord, Show)

mkEnv :: Trace IO (PABLogMsg ContractExe) -> Config -> IO AppEnv
mkEnv appTrace appConfig@Config { dbConfig
             , nodeServerConfig =  MockServerConfig{mscBaseUrl, mscSocketPath, mscSlotConfig}
             , walletServerConfig
             , chainIndexConfig
             } = do
    walletClientEnv <- clientEnv (Wallet.baseUrl walletServerConfig)
    nodeClientEnv <- clientEnv mscBaseUrl
    chainIndexEnv <- clientEnv (ChainIndex.ciBaseUrl chainIndexConfig)
    dbConnection <- dbConnect appTrace dbConfig
    txSendHandle <- liftIO $ MockClient.runTxSender mscSocketPath
    -- This is for access to the slot number in the interpreter
    chainSyncHandle <- liftIO $ MockClient.runChainSync' mscSocketPath mscSlotConfig
    appInMemContractStore <- liftIO initialInMemInstances
    pure AppEnv {..}
  where
    clientEnv baseUrl = mkClientEnv <$> liftIO mkManager <*> pure (coerce baseUrl)

    mkManager =
        newManager $
        tlsManagerSettings {managerModifyRequest = pure . setRequestIgnoreStatus}

logDebugString :: Trace IO (PABLogMsg t) -> Text -> IO ()
logDebugString trace = logDebug trace . SMultiAgent . UserLog

-- | Initialize/update the database to hold our effects.
migrate :: Trace IO (PABLogMsg ContractExe) -> DbConfig -> IO ()
migrate trace config = do
    connection <- dbConnect trace config
    logDebugString trace "Running beam migration"
    runBeamMigration trace connection

runBeamMigration
  ::
  Trace IO (PABLogMsg ContractExe)
  -> Sqlite.Connection
  -> IO ()
runBeamMigration trace conn = Sqlite.runBeamSqliteDebug (logDebugString trace . pack) conn $ do
  autoMigrate Sqlite.migrationBackend checkedSqliteDb

-- | Connect to the database.
dbConnect :: Trace IO (PABLogMsg ContractExe) -> DbConfig -> IO Sqlite.Connection
dbConnect trace DbConfig {dbConfigFile} = do
  logDebugString trace $ "Connecting to DB: " <> dbConfigFile
  open (unpack dbConfigFile)
