module API.Contract
  ( class ContractActivationId
  , activateContract
  , deactivateContract
  , getContractInstanceClientState
  , invokeEndpoint
  , getWalletContractInstances
  , getAllContractInstances
  , getContractDefinitions
  , defaultActivateContract
  , defaultDeactivateContract
  , defaultGetContractInstanceClientState
  , defaultInvokeEndpoint
  , defaultGetWalletContractInstances
  , defaultGetAllContractInstances
  , defaultGetContractDefinitions
  ) where

import Prelude
import API.Request (doGetRequest, doPostRequest, doPutRequest)
import API.Url (toUrlPiece)
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Foreign.Generic (class Decode, class Encode)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Webserver.Types (ContractActivationArgs, ContractInstanceClientState, ContractSignatureResponse)
import Servant.PureScript.Ajax (AjaxError)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Types (ContractInstanceId)

-- PAB contracts can be activated either with a `ContractExe` (a wrapper around a path to the exe on disk), or
-- some custom data type that identifies the contract (for versions of the plutus-pab that are bundled up with
-- contracts). That value is also returned in the `ContractInstanceClientState`. The implementation of the API
-- functions is the same regardless of this type, but for greater type safety we wrap them up in a class and
-- provide an instance for the `ContractExe`. To create an alternative type to use instead, simply make it an
-- instance of this class and give it all the default implementations.
-- Note that the implementation of some API functions is also the same regardless of the value of the type in
-- question that is passed, but we always have to pass one so that the compiler can determine the type of the
-- function.
class
  (Decode a, Encode a) <= ContractActivationId a where
  activateContract :: forall m. MonadError AjaxError m => MonadAff m => ContractActivationArgs a -> m ContractInstanceId
  deactivateContract :: forall m. MonadError AjaxError m => MonadAff m => a -> ContractInstanceId -> m Unit
  getContractInstanceClientState :: forall m. MonadError AjaxError m => MonadAff m => a -> ContractInstanceId -> m (ContractInstanceClientState a)
  invokeEndpoint :: forall d m. MonadError AjaxError m => MonadAff m => Encode d => a -> ContractInstanceId -> String -> d -> m Unit
  getWalletContractInstances :: forall m. MonadError AjaxError m => MonadAff m => a -> Wallet -> m (Array (ContractInstanceClientState a))
  getAllContractInstances :: forall m. MonadError AjaxError m => MonadAff m => a -> m (Array (ContractInstanceClientState a))
  getContractDefinitions :: forall m. MonadError AjaxError m => MonadAff m => a -> m (Array (ContractSignatureResponse a))

defaultActivateContract ::
  forall a m.
  ContractActivationId a =>
  MonadError AjaxError m =>
  MonadAff m =>
  ContractActivationArgs a -> m ContractInstanceId
defaultActivateContract contractActivationArgs = doPostRequest "/api/new/contract/activate" contractActivationArgs

defaultDeactivateContract ::
  forall a m.
  ContractActivationId a =>
  MonadError AjaxError m =>
  MonadAff m =>
  a -> ContractInstanceId -> m Unit
defaultDeactivateContract contractActivationId contractInstanceId = doPutRequest $ "api/new/contract/instance/" <> toUrlPiece contractInstanceId <> "/stop"

defaultGetContractInstanceClientState ::
  forall a m.
  ContractActivationId a =>
  MonadError AjaxError m =>
  MonadAff m =>
  a -> ContractInstanceId -> m (ContractInstanceClientState a)
defaultGetContractInstanceClientState contractActivationId contractInstanceId = doGetRequest $ "/api/new/contract/instance/" <> toUrlPiece contractInstanceId <> "/status"

defaultInvokeEndpoint ::
  forall a d m.
  ContractActivationId a =>
  Encode d =>
  MonadError AjaxError m =>
  MonadAff m =>
  a -> ContractInstanceId -> String -> d -> m Unit
defaultInvokeEndpoint contractActivationId contractInstanceId endpoint payload = doPostRequest ("/api/new/contract/instance/" <> toUrlPiece contractInstanceId <> "/endpoint/" <> endpoint) payload

defaultGetWalletContractInstances ::
  forall a m.
  ContractActivationId a =>
  MonadError AjaxError m =>
  MonadAff m =>
  a -> Wallet -> m (Array (ContractInstanceClientState a))
defaultGetWalletContractInstances contractActivationId wallet = doGetRequest $ "/api/new/contract/instances/wallet/" <> toUrlPiece wallet

defaultGetAllContractInstances ::
  forall a m.
  ContractActivationId a =>
  MonadError AjaxError m =>
  MonadAff m =>
  a -> m (Array (ContractInstanceClientState a))
defaultGetAllContractInstances contractActivationId = doGetRequest "/api/new/contract/instances"

defaultGetContractDefinitions ::
  forall a m.
  ContractActivationId a =>
  MonadError AjaxError m =>
  MonadAff m =>
  a -> m (Array (ContractSignatureResponse a))
defaultGetContractDefinitions contractActivationId = doGetRequest "/api/new/contract/definitions"
