{-# LANGUAGE RecordWildCards #-}
-- | Servant client for PAB
module Plutus.PAB.Webserver.Client (
    PabClient(..)
  , InstanceClient(..)
  , pabClient
) where

import           Data.Aeson                 (FromJSON, ToJSON (..))
import qualified Data.Aeson                 as JSON

import           Servant.API
import           Servant.Client

import           Plutus.PAB.Events.Contract
import           Plutus.PAB.Webserver.API
import           Plutus.PAB.Webserver.Types

import           Data.Proxy

-- | Client for PAB. The first type-argument is contract type that is used for PAB-simulator.
data PabClient t walletId = PabClient
  { activateContract :: ContractActivationArgs t -> ClientM ContractInstanceId
      -- ^ call activate contract method
  , instanceClient   :: ContractInstanceId -> InstanceClient t
      -- ^ call methods for instance client
  , getWallet        :: walletId -> ClientM [ContractInstanceClientState t]
      -- ^ get wallet
  , getInstances     :: ClientM [ContractInstanceClientState t]
      -- ^ get instance
  , getDefinitions   :: ClientM [ContractSignatureResponse t]
      -- ^ get definitions
  }

-- | Contract instance endpoints
data InstanceClient t = InstanceClient
  { getInstanceStatus    :: ClientM (ContractInstanceClientState t)
      -- ^ get instance status
  , callInstanceEndpoint :: String -> JSON.Value -> ClientM ()
      -- ^ call instance endpoint
  , stopInstance         :: ClientM ()
      -- ^ call stop instance method
  }

-- | Init generic pab client
pabClient :: (ToJSON t, FromJSON t, ToHttpApiData walletId) => PabClient t walletId
pabClient = PabClient{..}
  where
    (activateContract
      :<|> toInstanceClient
      :<|> getWallet
      :<|> getInstances
      :<|> getDefinitions
      ) = client (Proxy :: Proxy (NewAPI t walletId))

    instanceClient cid = InstanceClient{..}
        where
          getInstanceStatus :<|> callInstanceEndpoint :<|> stopInstance = toInstanceClient cid

