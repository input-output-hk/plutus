{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Cardano.Wallet.Server
    ( main
    ) where

import           Cardano.BM.Data.Trace               (Trace)
import qualified Cardano.ChainIndex.Client           as ChainIndexClient
import           Cardano.ChainIndex.Types            (ChainIndexUrl (..))
import qualified Cardano.Protocol.Socket.Client      as Client
import qualified Cardano.Protocol.Socket.Mock.Client as MockClient
import           Cardano.Wallet.API                  (API)
import           Cardano.Wallet.Mock
import           Cardano.Wallet.Types                (Port (..), WalletConfig (..), WalletInfo (..), WalletMsg (..),
                                                      WalletUrl (..), Wallets, createWallet, multiWallet)
import           Control.Concurrent.Availability     (Availability, available)
import           Control.Concurrent.MVar             (MVar, newMVar)
import           Control.Monad.Freer                 (reinterpret2, runM)
import           Control.Monad.Freer.Error           (handleError)
import           Control.Monad.Freer.Extras.Log      (logInfo)
import           Control.Monad.Freer.Reader          (runReader)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Coerce                         (coerce)
import           Data.Function                       ((&))
import qualified Data.Map.Strict                     as Map
import           Data.Proxy                          (Proxy (Proxy))
import           Ledger                              (Block)
import           Ledger.Crypto                       (pubKeyHash)
import           Ledger.Fee                          (FeeConfig)
import           Ledger.TimeSlot                     (SlotConfig)
import           Network.HTTP.Client                 (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp            as Warp
import           Plutus.PAB.Arbitrary                ()
import qualified Plutus.PAB.Monitoring.Monitoring    as LM
import           Servant                             (Application, NoContent (..), hoistServer, serve, (:<|>) ((:<|>)))
import           Servant.Client                      (BaseUrl (baseUrlPort), ClientEnv, ClientError, mkClientEnv)
import           Wallet.Effects                      (balanceTx, ownPubKey, startWatching, submitTxn, totalFunds,
                                                      walletAddSignature)
import           Wallet.Emulator.Wallet              (Wallet (..), emptyWalletState)
import qualified Wallet.Emulator.Wallet              as Wallet

app :: Trace IO WalletMsg
    -> MockClient.TxSendHandle
    -> Client.ChainSyncHandle Block
    -> ClientEnv
    -> MVar Wallets
    -> FeeConfig
    -> SlotConfig
    -> Application
app trace txSendHandle chainSyncHandle chainIndexEnv mVarState feeCfg slotCfg =
    serve (Proxy @(API Integer)) $
    hoistServer
        (Proxy @(API Integer))
        (processWalletEffects trace txSendHandle chainSyncHandle chainIndexEnv mVarState feeCfg slotCfg) $
            createWallet :<|>
            (\w tx -> multiWallet (Wallet w) (submitTxn tx) >>= const (pure NoContent)) :<|>
            (\w -> (\pk -> WalletInfo{wiWallet = Wallet w, wiPubKey = pk, wiPubKeyHash = pubKeyHash pk}) <$> multiWallet (Wallet w) ownPubKey) :<|>
            (\w -> multiWallet (Wallet w) . balanceTx) :<|>
            (\w -> multiWallet (Wallet w) totalFunds) :<|>
            (\w tx -> multiWallet (Wallet w) (walletAddSignature tx))

main :: Trace IO WalletMsg -> WalletConfig -> FeeConfig -> FilePath -> SlotConfig -> ChainIndexUrl -> Availability -> IO ()
main trace WalletConfig { baseUrl, wallet } feeCfg serverSocket slotCfg (ChainIndexUrl chainUrl) availability = LM.runLogEffects trace $ do
    chainIndexEnv <- buildEnv chainUrl defaultManagerSettings
    let knownWallets = Map.fromList $ (\w -> (w, emptyWalletState w)) . Wallet.Wallet <$> [1..10]
    mVarState <- liftIO $ newMVar knownWallets
    txSendHandle    <- liftIO $ MockClient.runTxSender   serverSocket
    chainSyncHandle <- liftIO $ MockClient.runChainSync' serverSocket slotCfg
    runClient chainIndexEnv
    logInfo $ StartingWallet (Port servicePort)
    liftIO $ Warp.runSettings warpSettings
           $ app trace
                 txSendHandle
                 chainSyncHandle
                 chainIndexEnv
                 mVarState
                 feeCfg
                 slotCfg
    where
        servicePort = baseUrlPort (coerce baseUrl)
        warpSettings = Warp.defaultSettings & Warp.setPort servicePort & Warp.setBeforeMainLoop (available availability)

        buildEnv url settings = liftIO
            $ newManager settings >>= \mgr -> pure $ mkClientEnv mgr url

        runClient env = liftIO
             $ runM
             $ flip handleError (error . show @ClientError)
             $ runReader env
             $ reinterpret2 ChainIndexClient.handleChainIndexClient
             $ startWatching (Wallet.ownAddress (emptyWalletState wallet))
