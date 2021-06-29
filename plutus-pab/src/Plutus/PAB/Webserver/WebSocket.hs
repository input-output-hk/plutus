{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-

Handlers for the websockets exposed by the PAB.

-}
module Plutus.PAB.Webserver.WebSocket
    ( wsHandler
    , combinedWebsocket
    , contractInstanceUpdates
    -- * Reports
    , getContractReport
    -- * Streams
    , STMStream
    , readOne
    , readN
    , foldM
    , unfold
    -- ** Streams of PAB events
    , walletFundsChange
    , openEndpoints
    , slotChange
    , observableStateChange
    ) where

import qualified Cardano.Wallet.Mock                     as Mock
import           Control.Applicative                     (Alternative (..), Applicative (..))
import           Control.Concurrent.Async                (Async, async, waitAnyCancel)
import           Control.Concurrent.STM                  (STM)
import qualified Control.Concurrent.STM                  as STM
import           Control.Exception                       (SomeException, handle)
import           Control.Monad                           (forever, guard, void)
import           Control.Monad.Freer.Error               (throwError)
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Aeson                              (ToJSON)
import qualified Data.Aeson                              as JSON
import           Data.Bifunctor                          (Bifunctor (..))
import           Data.Foldable                           (fold, traverse_)
import qualified Data.Map                                as Map
import           Data.Proxy                              (Proxy (..))
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import qualified Ledger
import           Ledger.Slot                             (Slot)
import qualified Network.WebSockets                      as WS
import           Network.WebSockets.Connection           (Connection, PendingConnection)
import           Numeric.Natural                         (Natural)
import           Plutus.Contract.Effects                 (ActiveEndpoint (..))
import           Plutus.PAB.Core                         (PABAction)
import qualified Plutus.PAB.Core                         as Core
import           Plutus.PAB.Core.ContractInstance.STM    (BlockchainEnv, InstancesState, OpenEndpoint (..))
import qualified Plutus.PAB.Core.ContractInstance.STM    as Instances
import qualified Plutus.PAB.Effects.Contract             as Contract
import           Plutus.PAB.Events.ContractInstanceState (fromResp)
import           Plutus.PAB.Types                        (PABError (OtherError))
import           Plutus.PAB.Webserver.API                ()
import           Plutus.PAB.Webserver.Types              (CombinedWSStreamToClient (..), CombinedWSStreamToServer (..),
                                                          ContractReport (..), ContractSignatureResponse (..),
                                                          InstanceStatusToClient (..))
import           Servant                                 ((:<|>) ((:<|>)))
import           Wallet.Emulator.Wallet                  (Wallet (..))
import qualified Wallet.Emulator.Wallet                  as Wallet
import           Wallet.Types                            (ContractInstanceId (..))

getContractReport :: forall t env. Contract.PABContract t => PABAction t env (ContractReport (Contract.ContractDef t))
getContractReport = do
    availableContracts <- Contract.getDefinitions @t
    activeContractIDs <- fmap fst . Map.toList <$> Contract.getActiveContracts @t
    crAvailableContracts <-
        traverse
            (\t -> ContractSignatureResponse t <$> Contract.exportSchema @t t)
            availableContracts
    crActiveContractStates <- traverse (\i -> Contract.getState @t i >>= \s -> pure (i, fromResp $ Contract.serialisableState (Proxy @t) s)) activeContractIDs
    pure ContractReport {crAvailableContracts, crActiveContractStates}

-- | An STM stream of 'a's (poor man's pull-based FRP)
newtype STMStream a = STMStream{ unSTMStream :: STM (a, Maybe (STMStream a)) }
    deriving Functor

-- | Join a stream of streams by producing values from the latest stream only.
joinStream :: STMStream (STMStream a) -> STMStream a
joinStream STMStream{unSTMStream} = STMStream $ unSTMStream >>= go where

    go :: (STMStream a, Maybe (STMStream (STMStream a))) -> STM (a, Maybe (STMStream a))
    go (STMStream currentStream, Nothing) = currentStream
    go (STMStream currentStream, Just (STMStream nextStream)) = do
        vl <- fmap Left currentStream <|> fmap Right nextStream
        case vl of
            Left (a, Just currentStream') -> pure (a, Just $ STMStream $ go (currentStream', Just (STMStream nextStream)))
            Left (a, Nothing) -> pure (a, Just $ joinStream $ STMStream nextStream)
            Right (newStream, Just nextStream') -> go (newStream, Just nextStream')
            Right (newStream, Nothing) -> go (newStream, Nothing)

singleton :: STM a -> STMStream a
singleton = STMStream . fmap (, Nothing)

instance Applicative STMStream where
    pure = singleton . pure
    -- | Updates when one of the two sides updates
    liftA2 :: forall a b c. (a -> b -> c) -> STMStream a -> STMStream b -> STMStream c
    liftA2 f (STMStream l) (STMStream r) = STMStream $ do
        x <- (,) <$> l <*> r
        let go :: ((a, Maybe (STMStream a)), (b, Maybe (STMStream b))) -> STM (c, Maybe (STMStream c))

            go ((currentL, Just (STMStream restL)), (currentR, Just (STMStream restR))) = do
                let next = do
                        v <- fmap Left restL <|> fmap Right restR
                        case v of
                            Left (newL, restL')  -> go ((newL, restL'), (currentR, Just $ STMStream restR))
                            Right (newR, restR') -> go ((currentL, Just $ STMStream restL), (newR, restR'))
                pure (f currentL currentR, Just $ STMStream next)
            go ((currentL, Just (STMStream restL)), (currentR, Nothing)) =
                let apply = flip f currentR in
                pure (f currentL currentR, Just $ STMStream $ fmap (first apply . second (fmap (fmap apply))) restL)
            go ((currentL, Nothing), (currentR, Just (STMStream restR))) =
                let apply = f currentL in
                pure (f currentL currentR, Just $ STMStream $ fmap (first apply . second (fmap (fmap apply))) restR)
            go ((currentL, Nothing), (currentR, Nothing)) =
                pure (f currentL currentR, Nothing)
        go x

instance Monad STMStream where
    x >>= f = joinStream (f <$> x)

instance Semigroup (STMStream a) where
    (STMStream l) <> (STMStream r) =
        STMStream $ do
            next <- fmap Left l <|> fmap Right r
            case next of
                Left (v, k)  -> pure (v, k <> Just (STMStream r))
                Right (v, k) -> pure (v, Just (STMStream l) <> k)

instance Monoid (STMStream a) where
    mappend = (<>)
    mempty = STMStream STM.retry

unfold :: Eq a => STM a -> STMStream a
unfold tx = STMStream $ go Nothing where
    go lastVl = do
        next <- tx
        traverse_ (\previous -> guard (previous /= next)) lastVl
        pure (next, Just $ STMStream $ go (Just next))

combinedUpdates :: forall t env. WSState -> PABAction t env (STMStream CombinedWSStreamToClient)
combinedUpdates wsState =
    combinedWSStreamToClient wsState
        <$> (Core.askBlockchainEnv @t @env)
        <*> (Core.askInstancesState @t @env)

-- | The subscriptions for a websocket (wallet funds and contract instance notifications)
data WSState = WSState
    { wsInstances :: STM.TVar (Set ContractInstanceId) -- ^ Contract instances that we want updates for
    , wsWallets   :: STM.TVar (Set Wallet) -- ^ Wallets whose funds we are watching
    }

instancesAndWallets :: WSState -> STMStream (Set ContractInstanceId, Set Wallet)
instancesAndWallets WSState{wsInstances, wsWallets} =
    (,) <$> unfold (STM.readTVar wsInstances) <*> unfold (STM.readTVar wsWallets)

combinedWSStreamToClient :: WSState -> BlockchainEnv -> InstancesState -> STMStream CombinedWSStreamToClient
combinedWSStreamToClient wsState blockchainEnv instancesState = do
    (instances, wallets) <- instancesAndWallets wsState
    let mkWalletStream wallet = WalletFundsChange wallet <$> walletFundsChange wallet blockchainEnv
        mkInstanceStream instanceId = InstanceUpdate instanceId <$> instanceUpdates instanceId instancesState
    fold
        [ SlotChange <$> slotChange blockchainEnv
        , foldMap mkWalletStream wallets
        , foldMap mkInstanceStream instances
        ]

initialWSState :: STM WSState
initialWSState = WSState <$> STM.newTVar mempty <*> STM.newTVar mempty

slotChange :: BlockchainEnv -> STMStream Slot
slotChange = unfold . Instances.currentSlot

walletFundsChange :: Wallet -> BlockchainEnv -> STMStream Ledger.Value
-- TODO: Change from 'Wallet' to 'Address' (see SCP-2208)
walletFundsChange wallet blockchainEnv =
    let addr = if Wallet.isEmulatorWallet wallet
                then Wallet.walletAddress wallet
                else Ledger.pubKeyHashAddress (Mock.walletPubKey wallet)
    in unfold (Instances.valueAt addr blockchainEnv)

observableStateChange :: ContractInstanceId -> InstancesState -> STMStream JSON.Value
observableStateChange contractInstanceId instancesState =
    unfold (Instances.obervableContractState contractInstanceId instancesState)

openEndpoints :: ContractInstanceId -> InstancesState -> STMStream [ActiveEndpoint]
openEndpoints contractInstanceId instancesState = STMStream $ do
    instanceState <- Instances.instanceState contractInstanceId instancesState
    let tx = fmap (oepName . snd) . Map.toList <$> Instances.openEndpoints instanceState
    initial <- tx
    pure (initial, Just (unfold tx))

finalValue :: ContractInstanceId -> InstancesState -> STMStream (Maybe JSON.Value)
finalValue contractInstanceId instancesState =
    singleton $ Instances.finalResult contractInstanceId instancesState

-- | Get a stream of instance updates for a given instance ID
instanceUpdates :: ContractInstanceId -> InstancesState -> STMStream InstanceStatusToClient
instanceUpdates instanceId instancesState =
    fold
        [ NewObservableState <$> observableStateChange instanceId instancesState
        , NewActiveEndpoints <$> openEndpoints instanceId instancesState
        , ContractFinished   <$> finalValue instanceId instancesState
        ]

-- | Read the first event from the stream
readOne :: STMStream a -> IO (a, Maybe (STMStream a))
readOne STMStream{unSTMStream} = STM.atomically unSTMStream

-- | Read a number of events from the stream. Blocks until all events
--   have been received.
readN :: Natural -> STMStream a -> IO [a]
readN 0 _ = pure []
readN k s = do
    (a, s') <- readOne s
    case s' of
        Nothing   -> pure [a]
        Just rest -> (:) a <$> readN (pred k) rest

-- | Consume a stream. Blocks until the stream has terminated.
foldM ::
    STMStream a -- ^ The stream
    -> (a -> IO ()) -- ^ Event handler
    -> IO () -- ^ Handler for the end of the stream
    -> IO ()
foldM s handleEvent handleStop = do
    (v, next) <- readOne s
    handleEvent v
    case next of
        Nothing -> handleStop
        Just s' -> foldM s' handleEvent handleStop

-- | Send all updates from an 'STMStream' to a websocket until it finishes.
streamToWebsocket :: forall t env a. ToJSON a => Connection -> STMStream a -> PABAction t env ()
streamToWebsocket connection stream = liftIO $
    foldM stream (WS.sendTextData connection . JSON.encode) (pure ())

-- | Handler for WSAPI
wsHandler ::
    forall t env.
    (ContractInstanceId -> PendingConnection -> PABAction t env ())
    :<|> (PendingConnection -> PABAction t env ())
wsHandler =
    contractInstanceUpdates :<|> combinedWebsocket

sendContractInstanceUpdatesToClient :: forall t env. ContractInstanceId -> Connection -> PABAction t env ()
sendContractInstanceUpdatesToClient instanceId connection = do
    instancesState <- Core.askInstancesState @t @env
    streamToWebsocket connection (instanceUpdates instanceId instancesState)

contractInstanceUpdates :: forall t env. ContractInstanceId -> PendingConnection -> PABAction t env ()
contractInstanceUpdates contractInstanceId pending = do
    Core.PABRunner{Core.runPABAction} <- Core.pabRunner
    liftIO $ do
        connection <- WS.acceptRequest pending
        handle disconnect . WS.withPingThread connection 30 (pure ()) $ fmap (either (error . show) id) . runPABAction $ sendContractInstanceUpdatesToClient contractInstanceId connection
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

combinedWebsocket :: forall t env. PendingConnection -> PABAction t env ()
combinedWebsocket pending = do
    pabRunner <- Core.pabRunner
    wsState <- liftIO $ STM.atomically initialWSState
    liftIO $ do
        connection <- WS.acceptRequest pending
        handle disconnect . WS.withPingThread connection 30 (pure ()) $ combinedWebsocketThread pabRunner wsState connection
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

combinedWebsocketThread :: forall t env. Core.PABRunner t env -> WSState -> Connection -> IO ()
combinedWebsocketThread Core.PABRunner{Core.runPABAction} wsState connection = do
        tasks :: [Async (Either PABError ())] <-
            traverse
                asyncApp
                [ sendCombinedUpdatesToClient connection wsState
                , receiveMessagesFromClient connection wsState
                ]
        void $ waitAnyCancel tasks
    where
        asyncApp = async . runPABAction

sendCombinedUpdatesToClient :: forall t env. Connection -> WSState -> PABAction t env ()
sendCombinedUpdatesToClient connection wsState = combinedUpdates wsState >>= streamToWebsocket connection

receiveMessagesFromClient :: forall t env. Connection -> WSState -> PABAction t env ()
receiveMessagesFromClient connection wsState = forever $ do
    msg <- liftIO $ WS.receiveData connection
    let result :: Either Text CombinedWSStreamToServer
        result = first Text.pack $ JSON.eitherDecode msg
    case result of
        Right (Subscribe l)   -> liftIO $ STM.atomically $ either (addInstanceId wsState) (addWallet wsState) l
        Right (Unsubscribe l) -> liftIO $ STM.atomically $ either (removeInstanceId wsState) (removeWallet wsState) l
        Left e                -> throwError (OtherError e)

addInstanceId :: WSState -> ContractInstanceId -> STM ()
addInstanceId WSState{wsInstances} i = STM.modifyTVar wsInstances (Set.insert i)

addWallet :: WSState -> Wallet -> STM ()
addWallet WSState{wsWallets} w = STM.modifyTVar wsWallets (Set.insert w)

removeInstanceId :: WSState -> ContractInstanceId -> STM ()
removeInstanceId WSState{wsInstances} i = STM.modifyTVar wsInstances (Set.delete i)

removeWallet :: WSState -> Wallet -> STM ()
removeWallet WSState{wsWallets} w = STM.modifyTVar wsWallets (Set.delete w)
