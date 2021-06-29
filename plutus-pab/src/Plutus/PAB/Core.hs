{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-

The core of the PAB. Runs contract instances, handling their requests by
talking to external services (wallet backend, chain index) and forwarding
notifications from node and user endpoints to them.

The main entry point is 'runPAB'. It runs a 'PABAction t env' using the
@EffectHandlers@ provided. The @EffectHandlers@ makes it easy to customise
the handlers for external effects and for actually submitting requests
to compiled contracts. By choosing different @EffectHandlers@ we can use the
same @runPAB@ function for test and live environments.

A number @PABAction@s are defined in this module. The most important one is
@activateContract@, which starts a new contract instance.

Another important @PABAction@ is 'Plutus.PAB.Core.Server.startServer', which
starts a webserver that implements the API defined in
'Plutus.PAB.Webserver.API'.

-}
module Plutus.PAB.Core
    ( PABEffects
    , PABAction
    , EffectHandlers(..)
    , runPAB
    , runPAB'
    , PABEnvironment(appEnv)
    -- * Contracts and instances
    , reportContractState
    , activateContract
    , activateContract'
    , callEndpointOnInstance
    , callEndpointOnInstance'
    , payToPublicKey
    -- * Agent threads
    , ContractInstanceEffects
    , handleAgentThread
    , stopInstance
    , instanceActivity
    -- * Querying the state
    , instanceState
    , observableState
    , waitForState
    , waitForTxConfirmed
    , activeEndpoints
    , waitForEndpoint
    , currentSlot
    , waitUntilSlot
    , waitNSlots
    , activeContracts
    , finalResult
    , waitUntilFinished
    , blockchainEnv
    , valueAtSTM
    , valueAt
    , askUserEnv
    , askBlockchainEnv
    , askInstancesState
    , runningInstances
    -- * Run PAB effects in separate threads
    , PABRunner(..)
    , pabRunner
    -- * Effect handlers
    , handleMappedReader
    , handleUserEnvReader
    , handleBlockchainEnvReader
    , handleInstancesStateReader
    , timed
    ) where

import           Control.Applicative                     (Alternative (..))
import           Control.Concurrent.STM                  (STM)
import qualified Control.Concurrent.STM                  as STM
import           Control.Monad                           (forM, guard, void)
import           Control.Monad.Freer                     (Eff, LastMember, Member, interpret, reinterpret, runM, send,
                                                          subsume, type (~>))
import           Control.Monad.Freer.Error               (Error, runError, throwError)
import           Control.Monad.Freer.Extras.Log          (LogMessage, LogMsg (..), LogObserve, handleObserveLog, mapLog)
import qualified Control.Monad.Freer.Extras.Modify       as Modify
import           Control.Monad.Freer.Reader              (Reader (..), ask, asks, runReader)
import           Control.Monad.IO.Class                  (MonadIO (..))
import qualified Data.Aeson                              as JSON
import           Data.Foldable                           (traverse_)
import qualified Data.Map                                as Map
import           Data.Proxy                              (Proxy (..))
import           Data.Set                                (Set)
import           Data.Text                               (Text)
import           Ledger.Tx                               (Address, Tx)
import           Ledger.TxId                             (TxId)
import           Ledger.Value                            (Value)
import           Plutus.Contract.Effects                 (ActiveEndpoint (..), PABReq, TxConfirmed)
import           Plutus.PAB.Core.ContractInstance        (ContractInstanceMsg, ContractInstanceState)
import qualified Plutus.PAB.Core.ContractInstance        as ContractInstance
import           Plutus.PAB.Core.ContractInstance.STM    (Activity (Active), BlockchainEnv, InstancesState,
                                                          OpenEndpoint (..))
import qualified Plutus.PAB.Core.ContractInstance.STM    as Instances
import           Plutus.PAB.Effects.Contract             (ContractDefinition, ContractEffect, ContractStore,
                                                          PABContract (..), getState)
import qualified Plutus.PAB.Effects.Contract             as Contract
import qualified Plutus.PAB.Effects.ContractRuntime      as ContractRuntime
import           Plutus.PAB.Effects.TimeEffect           (TimeEffect (..), systemTime)
import           Plutus.PAB.Effects.UUID                 (UUIDEffect, handleUUIDEffect)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse, fromResp)
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg (..))
import           Plutus.PAB.Timeout                      (Timeout)
import qualified Plutus.PAB.Timeout                      as Timeout
import           Plutus.PAB.Types                        (PABError (ContractInstanceNotFound, InstanceAlreadyStopped, WalletError))
import           Plutus.PAB.Webserver.Types              (ContractActivationArgs (..))
import           Wallet.API                              (PubKey, Slot)
import qualified Wallet.API                              as WAPI
import           Wallet.Effects                          (ChainIndexEffect, ContractRuntimeEffect, NodeClientEffect,
                                                          WalletEffect)
import           Wallet.Emulator.LogMessages             (RequestHandlerLogMsg, TxBalanceMsg)
import           Wallet.Emulator.MultiAgent              (EmulatorEvent' (..), EmulatorTimeEvent (..))
import           Wallet.Emulator.Wallet                  (Wallet, WalletEvent (..))
import           Wallet.Types                            (ContractInstanceId, EndpointDescription (..),
                                                          NotificationError)

-- | Effects that are available in 'PABAction's.
type PABEffects t env =
    '[ ContractStore t
     , ContractEffect t
     , ContractDefinition t
     , LogMsg (PABMultiAgentMsg t)
     , TimeEffect
     , Reader (PABEnvironment t env)
     , Error PABError
     , IO
     ]

-- | Actions that are run by the PAB.
type PABAction t env a = Eff (PABEffects t env) a

-- | A handler for 'PABAction' types.
newtype PABRunner t env = PABRunner { runPABAction :: forall a. PABAction t env a -> IO (Either PABError a) }

-- | Get a 'PABRunner' that uses the current environment.
pabRunner :: forall t env. PABAction t env (PABRunner t env)
pabRunner = do
    h@PABEnvironment{effectHandlers=EffectHandlers{handleLogMessages, handleContractStoreEffect, handleContractEffect, handleContractDefinitionEffect}} <- ask @(PABEnvironment t env)
    pure $ PABRunner $ \action -> do
        runM
            $ runError
            $ runReader h
            $ interpret (handleTimeEffect @t @env)
            $ handleLogMessages
            $ handleContractDefinitionEffect
            $ handleContractEffect
            $ handleContractStoreEffect action

-- | Shared data that is needed by all PAB threads.
data PABEnvironment t env =
    PABEnvironment
        { instancesState  :: InstancesState
        -- | How long to wait for an endpoint to become active before throwing the
        --   'EndpointNotAvailable' error.
        , endpointTimeout :: Timeout
        , blockchainEnv   :: BlockchainEnv
        , appEnv          :: env
        , effectHandlers  :: EffectHandlers t env
        }

-- | Top-level entry point. Run a 'PABAction', using the 'EffectHandlers' to
--   deal with logs, startup and shutdown, contract requests and communication
--   with external services.
runPAB ::
    forall t env a.
    Timeout
    -> EffectHandlers t env
    -> PABAction t env a
    -> IO (Either PABError a)
runPAB endpointTimeout effectHandlers action = runM $ runError $ do
    let EffectHandlers{initialiseEnvironment, onStartup, onShutdown, handleLogMessages, handleContractStoreEffect, handleContractEffect, handleContractDefinitionEffect} = effectHandlers
    (instancesState, blockchainEnv, appEnv) <- initialiseEnvironment
    let env = PABEnvironment{instancesState, blockchainEnv, appEnv, effectHandlers, endpointTimeout}

    runReader env $ interpret (handleTimeEffect @t @env) $ handleLogMessages $ handleContractDefinitionEffect $ handleContractEffect $ handleContractStoreEffect $ do
        onStartup
        result <- action
        onShutdown
        pure result

-- | Run a PABAction in the context of the given environment.
-- TODO: Clean it up so there is less duplication of the above.
runPAB' ::
    forall t env a.
    PABEnvironment t env
    -> PABAction t env a
    -> IO (Either PABError a)
runPAB' env@PABEnvironment{effectHandlers} action = runM $ runError $ do
    let EffectHandlers{onStartup, onShutdown, handleLogMessages, handleContractStoreEffect, handleContractEffect, handleContractDefinitionEffect} = effectHandlers

    runReader env $ interpret (handleTimeEffect @t @env) $ handleLogMessages $ handleContractDefinitionEffect $ handleContractEffect $ handleContractStoreEffect $ do
        onStartup
        result <- action
        onShutdown
        pure result

-- | Start a new instance of a contract, with a given state.
activateContract' :: forall t env. PABContract t => ContractInstanceState t -> ContractInstanceId -> Wallet -> ContractDef t -> PABAction t env ContractInstanceId
activateContract' state cid w def = do
    PABRunner{runPABAction} <- pabRunner

    let handler :: forall a. Eff (ContractInstanceEffects t env '[IO]) a -> IO a
        handler x = fmap (either (error . show) id) (runPABAction $ handleAgentThread w x)
        args :: ContractActivationArgs (ContractDef t)
        args = ContractActivationArgs{caWallet = w, caID = def}
    handleAgentThread w
        $ ContractInstance.activateContractSTM' @t @IO @(ContractInstanceEffects t env '[IO]) state cid handler args

-- | Start a new instance of a contract
activateContract :: forall t env. PABContract t => Wallet -> ContractDef t -> PABAction t env ContractInstanceId
activateContract w def = do
    PABRunner{runPABAction} <- pabRunner

    let handler :: forall a. Eff (ContractInstanceEffects t env '[IO]) a -> IO a
        handler x = fmap (either (error . show) id) (runPABAction $ handleAgentThread w x)
        args :: ContractActivationArgs (ContractDef t)
        args = ContractActivationArgs{caWallet = w, caID = def}
    handleAgentThread w
        $ ContractInstance.activateContractSTM @t @IO @(ContractInstanceEffects t env '[IO]) handler args

-- | Call a named endpoint on a contract instance. Waits if the endpoint is not
--   available.
callEndpointOnInstance ::
    forall t env a.
    ( JSON.ToJSON a
    )
    => ContractInstanceId
    -> String
    -> a
    -> PABAction t env (Maybe NotificationError)
callEndpointOnInstance instanceID ep value = do
    state <- asks @(PABEnvironment t env) instancesState
    timeoutVar <- asks @(PABEnvironment t env) endpointTimeout >>= liftIO . Timeout.startTimeout
    liftIO
        $ STM.atomically
        $ Instances.callEndpointOnInstanceTimeout timeoutVar state (EndpointDescription ep) (JSON.toJSON value) instanceID

-- | The 'InstanceState' for the instance. Throws a 'ContractInstanceNotFound' error if the instance does not exist.
instanceStateInternal :: forall t env. ContractInstanceId -> PABAction t env Instances.InstanceState
instanceStateInternal instanceId = do
    instancesState <- asks @(PABEnvironment t env) instancesState
    r <- liftIO $ STM.atomically $ (Left <$> Instances.instanceState instanceId instancesState) <|> (pure $ Right $ ContractInstanceNotFound instanceId)
    case r of
        Right err -> throwError err
        Left s    -> pure s

-- | Stop the instance.
stopInstance :: forall t env. ContractInstanceId -> PABAction t env ()
stopInstance instanceId = do
    Instances.InstanceState{Instances.issStatus, Instances.issStop} <- instanceStateInternal instanceId
    r' <- liftIO $ STM.atomically $ do
            status <- STM.readTVar issStatus
            case status of
                Active -> STM.putTMVar issStop () >> pure Nothing
                _      -> pure (Just $ InstanceAlreadyStopped instanceId)
    traverse_ throwError r'

-- | The 'Activity' of the instance.
instanceActivity :: forall t env. ContractInstanceId -> PABAction t env Activity
instanceActivity instanceId = do
    Instances.InstanceState{Instances.issStatus} <- instanceStateInternal instanceId
    liftIO $ STM.readTVarIO issStatus

-- | Call a named endpoint on a contract instance. Fails immediately if the
--   endpoint is not available.
callEndpointOnInstance' ::
    forall t env a.
    ( JSON.ToJSON a
    )
    => ContractInstanceId
    -> String
    -> a
    -> PABAction t env (Maybe NotificationError)
callEndpointOnInstance' instanceID ep value = do
    state <- asks @(PABEnvironment t env) instancesState
    liftIO
        $ STM.atomically
        $ Instances.callEndpointOnInstance state (EndpointDescription ep) (JSON.toJSON value) instanceID

-- | Make a payment to a public key
payToPublicKey :: Wallet -> PubKey -> Value -> PABAction t env Tx
payToPublicKey source target amount =
    handleAgentThread source
        $ Modify.wrapError WalletError
        $ WAPI.payToPublicKey WAPI.defaultSlotRange amount target

-- | Effects available to contract instances with access to external services.
type ContractInstanceEffects t env effs =
    ContractRuntimeEffect
    ': ContractEffect t
    ': ContractStore t
    ': WalletEffect
    ': ChainIndexEffect
    ': NodeClientEffect
    ': UUIDEffect
    ': LogMsg TxBalanceMsg
    ': LogMsg RequestHandlerLogMsg
    ': LogMsg (ContractInstanceMsg t)
    ': LogObserve (LogMessage Text)
    ': LogMsg Text
    ': Error PABError
    ': TimeEffect
    ': Reader BlockchainEnv
    ': Reader InstancesState
    ': Reader (PABEnvironment t env)
    ': Reader Wallet
    ': effs

-- | Handle an action with 'ContractInstanceEffects' in the context of a wallet.
handleAgentThread ::
    forall t env a.
    Wallet
    -> Eff (ContractInstanceEffects t env '[IO]) a
    -> PABAction t env a
handleAgentThread wallet action = do
    PABEnvironment{effectHandlers, blockchainEnv, instancesState} <- ask @(PABEnvironment t env)
    let EffectHandlers{handleContractStoreEffect, handleContractEffect, handleServicesEffects} = effectHandlers
    let action' :: Eff (ContractInstanceEffects t env (IO ': PABEffects t env)) a = Modify.raiseEnd action

    subsume @IO
        $ runReader wallet
        $ subsume @(Reader (PABEnvironment t env))
        $ runReader instancesState
        $ runReader blockchainEnv
        $ interpret (handleTimeEffect @t @env @IO)
        $ subsume @(Error PABError)
        $ (interpret (mapLog @_ @(PABMultiAgentMsg t) EmulatorMsg) . reinterpret (timed @EmulatorEvent') . reinterpret (mapLog (WalletEvent wallet)) . reinterpret (mapLog GenericLog))
        $ handleObserveLog
        $ interpret (mapLog ContractInstanceLog)
        $ (interpret (mapLog @_ @(PABMultiAgentMsg t) EmulatorMsg) . reinterpret (timed @EmulatorEvent') . reinterpret (mapLog (WalletEvent wallet)) . reinterpret (mapLog RequestHandlerLog))
        $ (interpret (mapLog @_ @(PABMultiAgentMsg t) EmulatorMsg) . reinterpret (timed @EmulatorEvent') . reinterpret (mapLog (WalletEvent wallet)) . reinterpret (mapLog TxBalanceLog))
        $ handleUUIDEffect
        $ handleServicesEffects wallet
        $ handleContractStoreEffect
        $ handleContractEffect
        $ (handleContractRuntimeMsg @t . reinterpret @ContractRuntimeEffect @(LogMsg ContractRuntime.ContractRuntimeMsg) ContractRuntime.handleContractRuntime) action'

-- | Effect handlers for running the PAB.
data EffectHandlers t env =
    EffectHandlers
        { -- | Create the initial environment. This value is shared between all threads
          --   started by the PAB.
          initialiseEnvironment :: forall effs.
            ( Member (Error PABError) effs
            , LastMember IO effs
            )
            => Eff effs (InstancesState, BlockchainEnv, env)

        -- | Handle log messages
        , handleLogMessages :: forall effs.
            ( Member (Reader (PABEnvironment t env)) effs
            , Member TimeEffect effs
            , Member (Error PABError) effs
            , LastMember IO effs
            )
            => Eff (LogMsg (PABMultiAgentMsg t) ': effs)
            ~> Eff effs

        -- | Handle the 'ContractStore' effect
        , handleContractStoreEffect :: forall effs.
            ( Member (Reader (PABEnvironment t env)) effs
            , Member (Error PABError) effs
            , Member TimeEffect effs
            , Member (LogMsg (PABMultiAgentMsg t)) effs
            , LastMember IO effs
            )
            => Eff (ContractStore t ': effs)
            ~> Eff effs

        -- | Handle the 'ContractEffect'
        , handleContractEffect :: forall effs.
            ( Member (Reader (PABEnvironment t env)) effs
            , Member (Error PABError) effs
            , Member TimeEffect effs
            , Member (LogMsg (PABMultiAgentMsg t)) effs
            , LastMember IO effs
            )
            => Eff (ContractEffect t ': effs)
            ~> Eff effs

        -- | Handle the 'ContractDefinition' effect
        , handleContractDefinitionEffect :: forall effs.
            ( Member (Reader (PABEnvironment t env)) effs
            , Member (Error PABError) effs
            , Member TimeEffect effs
            , Member (LogMsg (PABMultiAgentMsg t)) effs
            , LastMember IO effs
            )
            => Eff (ContractDefinition t ': effs)
            ~> Eff effs

        -- | Handle effects that serve requests to external services managed by the PAB
        --   Runs in the context of a particular wallet.
        , handleServicesEffects :: forall effs.
            ( Member (Reader (PABEnvironment t env)) effs
            , Member (Error PABError) effs
            , Member TimeEffect effs
            , Member (LogMsg (PABMultiAgentMsg t)) effs
            , LastMember IO effs
            )
            => Wallet
            -> Eff (WalletEffect ': ChainIndexEffect ': NodeClientEffect ': effs)
            ~> Eff effs

        -- | Action to run on startup.
        , onStartup :: PABAction t env ()

        -- | Action to run on shutdown
        , onShutdown :: PABAction t env ()
        }

-- | Report the state of a running contract.
reportContractState ::
    forall t effs.
    ( Member (ContractStore t) effs
    , PABContract t
    )
    => ContractInstanceId
    -> Eff effs (PartiallyDecodedResponse PABReq)
reportContractState cid = fromResp . Contract.serialisableState (Proxy @t) <$> getState @t cid

-- | Annotate log messages with the current slot number.
timed ::
    forall e effs.
    ( Member (LogMsg (EmulatorTimeEvent e)) effs
    , Member TimeEffect effs
    )
    => LogMsg e
    ~> Eff effs
timed = \case
    LMessage m -> do
        m' <- forM m $ \msg -> do
            sl <- systemTime
            pure (EmulatorTimeEvent sl msg)
        send (LMessage m')

handleContractRuntimeMsg :: forall t x effs. Member (LogMsg (PABMultiAgentMsg t)) effs => Eff (LogMsg ContractRuntime.ContractRuntimeMsg ': effs) x -> Eff effs x
handleContractRuntimeMsg = interpret (mapLog @_ @(PABMultiAgentMsg t) RuntimeLog)

-- | Get the current state of the contract instance.
instanceState :: forall t env. Wallet -> ContractInstanceId -> PABAction t env (Contract.State t)
instanceState wallet instanceId = handleAgentThread wallet (Contract.getState @t instanceId)

-- | An STM transaction that returns the observable state of the contract instance.
observableState :: forall t env. ContractInstanceId -> PABAction t env (STM JSON.Value)
observableState instanceId = do
    instancesState <- asks @(PABEnvironment t env) instancesState
    pure $ Instances.obervableContractState instanceId instancesState

-- | Wait until the observable state of the instance matches a predicate.
waitForState :: forall t env a. (JSON.Value -> Maybe a) -> ContractInstanceId -> PABAction t env a
waitForState extract instanceId = do
    stm <- observableState instanceId
    liftIO $ STM.atomically $ do
        state <- stm
        maybe STM.retry pure (extract state)

-- | Wait for the transaction to be confirmed on the blockchain.
waitForTxConfirmed :: forall t env. TxId -> PABAction t env TxConfirmed
waitForTxConfirmed t = do
    env <- asks @(PABEnvironment t env) blockchainEnv
    liftIO $ STM.atomically $ Instances.waitForTxConfirmed t env

-- | The list of endpoints that are currently open
activeEndpoints :: forall t env. ContractInstanceId -> PABAction t env (STM [OpenEndpoint])
activeEndpoints instanceId = do
    instancesState <- asks @(PABEnvironment t env) instancesState
    pure $ do
        is <- Instances.instanceState instanceId instancesState
        fmap snd . Map.toList <$> Instances.openEndpoints is

-- | Wait until the endpoint becomes active.
waitForEndpoint :: forall t env. ContractInstanceId -> String -> PABAction t env ()
waitForEndpoint instanceId endpointName = do
    tx <- activeEndpoints instanceId
    liftIO $ STM.atomically $ do
        eps <- tx
        guard $ any (\Instances.OpenEndpoint{Instances.oepName=ActiveEndpoint{aeDescription=EndpointDescription nm}} -> nm == endpointName) eps

currentSlot :: forall t env. PABAction t env (STM Slot)
currentSlot = do
    Instances.BlockchainEnv{Instances.beCurrentSlot} <- asks @(PABEnvironment t env) blockchainEnv
    pure $ STM.readTVar beCurrentSlot

-- | Wait until the target slot number has been reached
waitUntilSlot :: forall t env. Slot -> PABAction t env ()
waitUntilSlot targetSlot = do
    tx <- currentSlot
    void $ liftIO $ STM.atomically $ do
        s <- tx
        guard (s >= targetSlot)

waitNSlots :: forall t env. Int -> PABAction t env ()
waitNSlots i = do
    current <- currentSlot >>= liftIO . STM.atomically
    waitUntilSlot (current + fromIntegral i)

-- | The set of all active contracts.
activeContracts :: forall t env. PABAction t env (Set ContractInstanceId)
activeContracts = do
    instancesState <- asks @(PABEnvironment t env) instancesState
    liftIO $ STM.atomically $ Instances.instanceIDs instancesState

-- | The final result of the instance (waits until it is available)
finalResult :: forall t env. ContractInstanceId -> PABAction t env (STM (Maybe JSON.Value))
finalResult instanceId = do
    instancesState <- asks @(PABEnvironment t env) instancesState
    pure $ Instances.finalResult instanceId instancesState

-- | An STM transaction returning the value at an address
valueAtSTM :: forall t env. Address -> PABAction t env (STM Value)
valueAtSTM address = do
    blockchainEnv <- asks @(PABEnvironment t env) blockchainEnv
    return $ Instances.valueAt address blockchainEnv

-- | The value at an address
valueAt :: forall t env. Address -> PABAction t env Value
valueAt address = valueAtSTM address >>= liftIO . STM.atomically

-- | Wait until the contract is done, then return
--   the error (if any)
waitUntilFinished :: forall t env. ContractInstanceId -> PABAction t env (Maybe JSON.Value)
waitUntilFinished i = finalResult i >>= liftIO . STM.atomically

runningInstances :: forall t env. PABAction t env (Set ContractInstanceId)
runningInstances = askInstancesState @t @env >>= liftIO . STM.atomically . Instances.runningInstances

-- | Read the 'env' from the environment
askUserEnv :: forall t env effs. Member (Reader (PABEnvironment t env)) effs => Eff effs env
askUserEnv = interpret (handleUserEnvReader @t @env) ask

-- | Read the 'BlockchainEnv' from the environment
askBlockchainEnv :: forall t env effs. Member (Reader (PABEnvironment t env)) effs => Eff effs BlockchainEnv
askBlockchainEnv = interpret (handleBlockchainEnvReader @t @env) ask

-- | Read the 'InstancesState' from the environment
askInstancesState :: forall t env effs. Member (Reader (PABEnvironment t env)) effs => Eff effs InstancesState
askInstancesState = interpret (handleInstancesStateReader @t @env) ask

handleMappedReader :: forall f g effs.
    Member (Reader f) effs
    => (f -> g)
    -> Reader g
    ~> Eff effs
handleMappedReader f = \case
    Ask -> asks @f f

handleUserEnvReader :: forall t env effs.
    Member (Reader (PABEnvironment t env)) effs
    => Reader env
    ~> Eff effs
handleUserEnvReader = \case
    Ask -> asks @(PABEnvironment t env) appEnv

handleBlockchainEnvReader :: forall t env effs.
    Member (Reader (PABEnvironment t env)) effs
    => Reader BlockchainEnv
    ~> Eff effs
handleBlockchainEnvReader = \case
    Ask -> asks @(PABEnvironment t env) blockchainEnv

handleInstancesStateReader :: forall t env effs.
    Member (Reader (PABEnvironment t env)) effs
    => Reader InstancesState
    ~> Eff effs
handleInstancesStateReader = \case
    Ask -> asks @(PABEnvironment t env) instancesState

-- | Handle the 'TimeEffect' by reading the current slot number from
--   the blockchain env.
handleTimeEffect ::
    forall t env m effs.
    ( Member (Reader (PABEnvironment t env)) effs
    , LastMember m effs
    , MonadIO m
    )
    => TimeEffect
    ~> Eff effs
handleTimeEffect = \case
    SystemTime -> do
        Instances.BlockchainEnv{Instances.beCurrentSlot} <- asks @(PABEnvironment t env) blockchainEnv
        liftIO $ STM.readTVarIO beCurrentSlot
