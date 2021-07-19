module Capability.Marlowe
  ( class ManageMarlowe
  , createWallet
  , followContract
  , createPendingFollowerApp
  , followContractWithPendingFollowerApp
  , createContract
  , applyTransactionInput
  , redeem
  , lookupWalletInfo
  , lookupWalletDetails
  , getRoleContracts
  , getFollowerApps
  , subscribeToWallet
  , unsubscribeFromWallet
  , subscribeToPlutusApp
  , unsubscribeFromPlutusApp
  ) where

import Prelude
import API.Lenses (_cicContract, _cicCurrentState, _cicDefinition, _cicWallet, _observableState)
import Affjax (defaultRequest)
import AppM (AppM)
import Bridge (toBack, toFront)
import Capability.Contract (activateContract, getContractInstanceClientState, getContractInstanceObservableState, getWalletContractInstances, invokeEndpoint) as Contract
import Capability.Contract (class ManageContract)
import Capability.MarloweStorage (class ManageMarloweStorage, addAssets, getContracts, getWalletLibrary, getWalletRoleContracts, insertContract, insertWalletRoleContracts)
import Capability.Wallet (class ManageWallet)
import Capability.Wallet (createWallet, getWalletInfo, getWalletTotalFunds) as Wallet
import Capability.Websocket (class ManageWebsocket)
import Capability.Websocket (subscribeToContract, subscribeToWallet, unsubscribeFromContract, unsubscribeFromWallet) as Websocket
import Control.Monad.Except (ExceptT(..), except, lift, mapExceptT, runExcept, runExceptT, withExceptT)
import Control.Monad.Reader.Class (ask)
import Data.Array (filter) as Array
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.BigInteger (fromInt)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Lens (view)
import Data.Map (Map, findMin, fromFoldable, lookup, mapMaybeWithKey, singleton, toUnfoldable, values)
import Data.Map (filter) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID, parseUUID, toString)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Env (DataProvider(..), PABType(..))
import Foreign (MultipleErrors)
import Foreign.Generic (decodeJSON)
import Halogen (HalogenM, liftAff)
import MainFrame.Types (Msg)
import Marlowe.PAB (ContractHistory, MarloweData, MarloweParams, PlutusApp(..), PlutusAppId(..))
import Marlowe.Semantics (Assets(..), Contract, TokenName, TransactionInput(..), asset, emptyState)
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Plutus.V1.Ledger.Crypto (PubKeyHash) as Back
import Plutus.V1.Ledger.Value (CurrencySymbol(..))
import Plutus.V1.Ledger.Value (TokenName) as Back
import PlutusTx.AssocMap (Map) as Back
import Servant.PureScript.Ajax (AjaxError(..), ErrorDescription(..))
import Types (AjaxResponse, DecodedAjaxResponse)
import WalletData.Lenses (_companionAppId, _marloweAppId, _pubKey, _pubKeyHash, _wallet, _walletInfo)
import WalletData.Types (PubKeyHash(..), Wallet(..), WalletDetails, WalletInfo(..))

-- The `ManageMarlowe` class provides a window on the `ManageContract`, `ManageWallet`, and
-- `ManageWebsocket` capabilities with functions specific to Marlowe. Or rather, it does when the
-- `dataProvider` env variable is set to `PAB`. When it is set to `LocalStorage`, these functions
-- instead provide what is needed to mimic real PAB behaviour in the frontend.
-- TODO (possibly): make `AppM` a `MonadError` and remove all the `runExceptT`s
class
  (ManageContract m, ManageMarloweStorage m, ManageWallet m, ManageWebsocket m) <= ManageMarlowe m where
  createWallet :: m (AjaxResponse WalletDetails)
  followContract :: WalletDetails -> MarloweParams -> m (DecodedAjaxResponse (Tuple PlutusAppId ContractHistory))
  createPendingFollowerApp :: WalletDetails -> m (AjaxResponse PlutusAppId)
  followContractWithPendingFollowerApp :: WalletDetails -> MarloweParams -> PlutusAppId -> m (DecodedAjaxResponse (Tuple PlutusAppId ContractHistory))
  createContract :: WalletDetails -> Map TokenName PubKeyHash -> Contract -> m (AjaxResponse Unit)
  applyTransactionInput :: WalletDetails -> MarloweParams -> TransactionInput -> m (AjaxResponse Unit)
  redeem :: WalletDetails -> MarloweParams -> TokenName -> m (AjaxResponse Unit)
  lookupWalletInfo :: PlutusAppId -> m (AjaxResponse WalletInfo)
  lookupWalletDetails :: PlutusAppId -> m (AjaxResponse WalletDetails)
  getRoleContracts :: WalletDetails -> m (DecodedAjaxResponse (Map MarloweParams MarloweData))
  getFollowerApps :: WalletDetails -> m (DecodedAjaxResponse (Map PlutusAppId ContractHistory))
  subscribeToPlutusApp :: DataProvider -> PlutusAppId -> m Unit
  subscribeToWallet :: DataProvider -> Wallet -> m Unit
  unsubscribeFromPlutusApp :: DataProvider -> PlutusAppId -> m Unit
  unsubscribeFromWallet :: DataProvider -> Wallet -> m Unit

instance monadMarloweAppM :: ManageMarlowe AppM where
  -- create a Wallet, together with a WalletCompanion and a MarloweApp, and return the WalletDetails
  createWallet = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType -> do
        ajaxWalletInfo <- Wallet.createWallet
        case ajaxWalletInfo of
          Left ajaxError -> pure $ Left ajaxError
          Right walletInfo -> do
            let
              wallet = view _wallet walletInfo
            ajaxCompanionAppId <- case pabType of
              Plain -> Contract.activateContract WalletCompanion wallet
              WithMarloweContracts -> Contract.activateContract WalletCompanion wallet
            ajaxMarloweAppId <- case pabType of
              Plain -> Contract.activateContract MarloweApp wallet
              WithMarloweContracts -> Contract.activateContract MarloweApp wallet
            ajaxAssets <- Wallet.getWalletTotalFunds wallet
            let
              createWalletDetails companionAppId marloweAppId assets =
                { walletNickname: ""
                , companionAppId
                , marloweAppId
                , walletInfo
                , assets
                , previousCompanionAppState: Nothing
                }
            pure $ createWalletDetails <$> ajaxCompanionAppId <*> ajaxMarloweAppId <*> ajaxAssets
      LocalStorage -> do
        uuid <- liftEffect genUUID
        number <- liftEffect random
        let
          integer = floor $ number * 1000000000000000000.0

          walletInfo =
            WalletInfo
              { wallet: Wallet $ fromInt integer
              , pubKey: toString uuid
              , pubKeyHash: PubKeyHash $ toString uuid
              }

          assets = Assets $ singleton "" $ singleton "" (fromInt 1000000 * fromInt 10000)

          walletDetails =
            { walletNickname: ""
            , companionAppId: PlutusAppId uuid
            , marloweAppId: PlutusAppId uuid
            , walletInfo
            , assets
            , previousCompanionAppState: Nothing
            }
        pure $ Right walletDetails
  -- create a MarloweFollower app, call its "follow" endpoint with the given MarloweParams, and then
  -- return its PlutusAppId and observable state
  followContract walletDetails marloweParams = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          followAppId <-
            withExceptT Left $ ExceptT
              $ case pabType of
                  Plain -> Contract.activateContract MarloweFollower wallet
                  WithMarloweContracts -> Contract.activateContract MarloweFollower wallet
          void $ withExceptT Left $ ExceptT
            $ case pabType of
                Plain -> Contract.invokeEndpoint MarloweFollower followAppId "follow" marloweParams
                WithMarloweContracts -> Contract.invokeEndpoint MarloweFollower followAppId "follow" marloweParams
          observableStateJson <-
            withExceptT Left $ ExceptT
              $ case pabType of
                  Plain -> Contract.getContractInstanceObservableState MarloweFollower followAppId
                  WithMarloweContracts -> Contract.getContractInstanceObservableState MarloweFollower followAppId
          observableState <- mapExceptT (pure <<< lmap Right <<< unwrap) $ decodeJSON $ unwrap observableStateJson
          pure $ followAppId /\ observableState
      LocalStorage -> do
        existingContracts <- getContracts
        case lookup marloweParams existingContracts of
          Just (marloweData /\ transactionInputs) -> do
            uuid <- liftEffect genUUID
            let
              -- Note [MarloweParams]: In the PAB, the PlutusAppId and the MarloweParams are completely independent,
              -- and you can have several follower apps (with different PlutusAppIds) all following the same contract
              -- (identified by its MarloweParams). For the LocalStorage simlation we just have one follower app for
              -- each contract, and make its PlutusAppId a function of the MarloweParams. I thought this would be
              -- simpler, but it turned out to lead to a complication (see note [PendingContracts] in Dashboard.State).
              -- I'm not going to change it now though, because this LocalStorage stuff is temporary anyway, and will
              -- be removed when the PAB is working fully.
              mUuid = parseUUID marloweParams.rolePayoutValidatorHash

              followAppId = PlutusAppId $ fromMaybe uuid mUuid

              observableState = { chParams: Just (marloweParams /\ marloweData), chHistory: transactionInputs }
            pure $ Right $ followAppId /\ observableState
          Nothing -> pure $ Left $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- create a MarloweFollower app and return its PlutusAppId, but don't call its "follow" endpoint
  -- (this function is used for creating "placeholder" contracts before we know the MarloweParams)
  createPendingFollowerApp walletDetails = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType -> do
        let
          wallet = view (_walletInfo <<< _wallet) walletDetails
        case pabType of
          Plain -> Contract.activateContract MarloweFollower wallet
          WithMarloweContracts -> Contract.activateContract MarloweFollower wallet
      LocalStorage -> do
        uuid <- liftEffect genUUID
        pure $ Right $ PlutusAppId uuid
  -- call the "follow" endpoint of a pending MarloweFollower app, and return its PlutusAppId and
  -- observable state (to call this function, we must already know its PlutusAppId, but we return
  -- it anyway because it is convenient to have this function return the same type as
  -- `followContract`)
  followContractWithPendingFollowerApp walletDetails marloweParams followerAppId = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          void $ withExceptT Left $ ExceptT
            $ case pabType of
                Plain -> Contract.invokeEndpoint MarloweFollower followerAppId "follow" marloweParams
                WithMarloweContracts -> Contract.invokeEndpoint MarloweFollower followerAppId "follow" marloweParams
          observableStateJson <-
            withExceptT Left $ ExceptT
              $ case pabType of
                  Plain -> Contract.getContractInstanceObservableState MarloweFollower followerAppId
                  WithMarloweContracts -> Contract.getContractInstanceObservableState MarloweFollower followerAppId
          observableState <- mapExceptT (pure <<< lmap Right <<< unwrap) $ decodeJSON $ unwrap observableStateJson
          pure $ followerAppId /\ observableState
      LocalStorage -> do
        existingContracts <- getContracts
        case lookup marloweParams existingContracts of
          Just (marloweData /\ transactionInputs) -> do
            uuid <- liftEffect genUUID
            let
              -- See note [MarloweParams] above.
              mUuid = parseUUID marloweParams.rolePayoutValidatorHash

              correctedFollowerAppId = PlutusAppId $ fromMaybe uuid mUuid

              observableState = { chParams: Just (marloweParams /\ marloweData), chHistory: transactionInputs }
            pure $ Right $ correctedFollowerAppId /\ observableState
          Nothing -> pure $ Left $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- "create" a Marlowe contract on the blockchain
  -- FIXME: if we want users to be able to follow contracts that they don't have roles in, we need this function
  -- to return the MarloweParams of the created contract - but this isn't currently possible in the PAB
  -- UPDATE to this FIXME: it is possible this won't be a problem, as it seems role tokens are first paid into
  -- the wallet that created the contract, and distributed to other wallets from there - but this remains to be
  -- seen when all the parts are working together as they should be...
  createContract walletDetails roles contract = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType ->
        let
          marloweAppId = view _marloweAppId walletDetails

          bRoles :: Back.Map Back.TokenName Back.PubKeyHash
          bRoles = toBack roles
        in
          case pabType of
            Plain -> Contract.invokeEndpoint MarloweApp marloweAppId "create" (bRoles /\ contract)
            WithMarloweContracts -> Contract.invokeEndpoint MarloweApp marloweAppId "create" (bRoles /\ contract)
      LocalStorage -> do
        walletLibrary <- getWalletLibrary
        uuid <- liftEffect genUUID
        let
          marloweParams =
            { rolePayoutValidatorHash: toString uuid
            , rolesCurrency: CurrencySymbol { unCurrencySymbol: toString uuid }
            }

          marloweData =
            { marloweContract: contract
            , marloweState: emptyState zero
            }
        void $ insertContract marloweParams (marloweData /\ mempty)
        void $ insertWalletRoleContracts (view (_walletInfo <<< _pubKey) walletDetails) marloweParams marloweData
        let
          unfoldableRoles :: Array (Tuple TokenName PubKeyHash)
          unfoldableRoles = toUnfoldable roles
        void
          $ for unfoldableRoles \(tokenName /\ pubKeyHash) -> do
              void $ addAssets pubKeyHash $ asset (toString uuid) tokenName (fromInt 1)
              void $ insertWalletRoleContracts (unwrap pubKeyHash) marloweParams marloweData
        pure $ Right unit
  -- "apply-inputs" to a Marlowe contract on the blockchain
  applyTransactionInput walletDetails marloweParams transactionInput@(TransactionInput { interval, inputs }) = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType ->
        let
          marloweAppId = view _marloweAppId walletDetails
        in
          case pabType of
            Plain -> Contract.invokeEndpoint MarloweApp marloweAppId "apply-inputs" (marloweParams /\ Just interval /\ inputs)
            WithMarloweContracts -> Contract.invokeEndpoint MarloweApp marloweAppId "apply-inputs" (marloweParams /\ Just interval /\ inputs)
      LocalStorage -> do
        existingContracts <- getContracts
        -- When we emulate these calls we add a 500ms delay so we give time to the submit button
        -- to show a loading indicator (we'll remove this once the PAB is connected)
        liftAff $ delay $ Milliseconds 500.0
        case lookup marloweParams existingContracts of
          Just (marloweData /\ transactionInputs) -> do
            void $ insertContract marloweParams (marloweData /\ (transactionInputs <> [ transactionInput ]))
            pure $ Right unit
          Nothing -> pure $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- "redeem" payments from a Marlowe contract on the blockchain
  redeem walletDetails marloweParams tokenName = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType ->
        let
          marloweAppId = view _marloweAppId walletDetails

          pubKeyHash = view (_walletInfo <<< _pubKeyHash) walletDetails
        in
          case pabType of
            Plain -> Contract.invokeEndpoint MarloweApp marloweAppId "redeem" (marloweParams /\ tokenName /\ pubKeyHash)
            WithMarloweContracts -> Contract.invokeEndpoint MarloweApp marloweAppId "redeem" (marloweParams /\ tokenName /\ pubKeyHash)
      LocalStorage -> pure $ Right unit
  -- get the WalletInfo of a wallet given the PlutusAppId of its WalletCompanion
  lookupWalletInfo companionAppId = do
    { dataProvider } <- ask
    case dataProvider of
      PAB Plain ->
        runExceptT do
          clientState <- ExceptT $ Contract.getContractInstanceClientState WalletCompanion companionAppId
          let
            app = view _cicDefinition clientState
          case app of
            WalletCompanion -> do
              let
                wallet = toFront $ view _cicWallet clientState
              ExceptT $ Wallet.getWalletInfo wallet
            _ -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
      PAB WithMarloweContracts ->
        runExceptT do
          clientState <- ExceptT $ Contract.getContractInstanceClientState WalletCompanion companionAppId
          let
            app = view _cicDefinition clientState
          case app of
            WalletCompanion -> do
              let
                wallet = toFront $ view _cicWallet clientState
              ExceptT $ Wallet.getWalletInfo wallet
            _ -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
      LocalStorage ->
        runExceptT do
          walletDetails <- ExceptT $ lookupWalletDetails companionAppId
          pure $ view _walletInfo walletDetails
  -- get the WalletDetails of a wallet given the PlutusAppId of its WalletCompanion
  -- note: this returns an empty walletNickname (because these are only saved locally)
  lookupWalletDetails companionAppId = do
    { dataProvider } <- ask
    case dataProvider of
      PAB Plain ->
        runExceptT do
          clientState <- ExceptT $ Contract.getContractInstanceClientState WalletCompanion companionAppId
          let
            app = view _cicDefinition clientState
          case app of
            WalletCompanion -> do
              let
                wallet = toFront $ view _cicWallet clientState
              walletContracts <- ExceptT $ Contract.getWalletContractInstances WalletCompanion wallet
              walletInfo <- ExceptT $ Wallet.getWalletInfo wallet
              assets <- ExceptT $ Wallet.getWalletTotalFunds wallet
              case find (\state -> (view _cicDefinition state) == MarloweApp) walletContracts of
                Just marloweApp ->
                  ExceptT $ pure
                    $ Right
                        { walletNickname: mempty
                        , companionAppId
                        , marloweAppId: toFront $ view _cicContract marloweApp
                        , walletInfo
                        , assets
                        , previousCompanionAppState: Nothing
                        }
                Nothing -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
            _ -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
      PAB WithMarloweContracts ->
        runExceptT do
          clientState <- ExceptT $ Contract.getContractInstanceClientState WalletCompanion companionAppId
          let
            appType = view _cicDefinition clientState
          case appType of
            WalletCompanion -> do
              let
                wallet = toFront $ view _cicWallet clientState
              walletContracts <- ExceptT $ Contract.getWalletContractInstances WalletCompanion wallet
              walletInfo <- ExceptT $ Wallet.getWalletInfo wallet
              assets <- ExceptT $ Wallet.getWalletTotalFunds wallet
              case find (\state -> view _cicDefinition state == MarloweApp) walletContracts of
                Just marloweApp ->
                  ExceptT $ pure
                    $ Right
                        { walletNickname: mempty
                        , companionAppId
                        , marloweAppId: toFront $ view _cicContract marloweApp
                        , walletInfo
                        , assets
                        , previousCompanionAppState: Nothing
                        }
                Nothing -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
            _ -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
      LocalStorage -> do
        walletLibrary <- getWalletLibrary
        let
          mWalletDetails = findMin $ Map.filter (\walletDetails -> view _companionAppId walletDetails == companionAppId) walletLibrary
        case mWalletDetails of
          Just { key, value } -> pure $ Right value
          Nothing -> pure $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts walletDetails = do
    { dataProvider } <- ask
    case dataProvider of
      PAB pabType ->
        runExceptT do
          let
            companionAppId = view _companionAppId walletDetails
          observableStateJson <-
            withExceptT Left $ ExceptT
              $ case pabType of
                  Plain -> Contract.getContractInstanceObservableState WalletCompanion companionAppId
                  WithMarloweContracts -> Contract.getContractInstanceObservableState WalletCompanion companionAppId
          mapExceptT (pure <<< lmap Right <<< unwrap) $ decodeJSON $ unwrap observableStateJson
      LocalStorage -> do
        roleContracts <- getWalletRoleContracts $ view (_walletInfo <<< _pubKey) walletDetails
        pure $ Right roleContracts
  -- get all MarloweFollower apps for a given wallet
  getFollowerApps walletDetails = do
    { dataProvider } <- ask
    case dataProvider of
      PAB Plain ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          runningApps <- withExceptT Left $ ExceptT $ Contract.getWalletContractInstances MarloweFollower wallet
          let
            followerApps = Array.filter (\cic -> view _cicDefinition cic == MarloweFollower) runningApps
          case traverse decodeFollowerAppState (followerApps) of
            Left decodingError -> except $ Left $ Right decodingError
            Right decodedFollowerApps -> ExceptT $ pure $ Right $ fromFoldable decodedFollowerApps
        where
        decodeFollowerAppState :: ContractInstanceClientState PlutusApp -> Either MultipleErrors (Tuple PlutusAppId ContractHistory)
        decodeFollowerAppState contractInstanceClientState =
          let
            plutusAppId = toFront $ view _cicContract contractInstanceClientState

            rawJson = view (_cicCurrentState <<< _observableState) contractInstanceClientState
          in
            case runExcept $ decodeJSON $ unwrap rawJson of
              Left decodingErrors -> Left decodingErrors
              Right observableState -> Right (plutusAppId /\ observableState)
      PAB WithMarloweContracts ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          runningApps <- withExceptT Left $ ExceptT $ Contract.getWalletContractInstances MarloweFollower wallet
          let
            followerApps = Array.filter (\cic -> view _cicDefinition cic == MarloweFollower) runningApps
          case traverse decodeFollowerAppState followerApps of
            Left decodingError -> except $ Left $ Right decodingError
            Right decodedFollowerApps -> ExceptT $ pure $ Right $ fromFoldable decodedFollowerApps
        where
        decodeFollowerAppState :: ContractInstanceClientState PlutusApp -> Either MultipleErrors (Tuple PlutusAppId ContractHistory)
        decodeFollowerAppState contractInstanceClientState =
          let
            plutusAppId = toFront $ view _cicContract contractInstanceClientState

            rawJson = view (_cicCurrentState <<< _observableState) contractInstanceClientState
          in
            case runExcept $ decodeJSON $ unwrap rawJson of
              Left decodingErrors -> Left decodingErrors
              Right observableState -> Right (plutusAppId /\ observableState)
      LocalStorage -> do
        roleContracts <- getWalletRoleContracts $ view (_walletInfo <<< _pubKey) walletDetails
        allContracts <- getContracts
        let
          roleContractsToHistory :: MarloweParams -> MarloweData -> Maybe (Tuple PlutusAppId ContractHistory)
          roleContractsToHistory marloweParams marloweData =
            let
              -- See note [MarloweParams] above.
              mUuid = parseUUID marloweParams.rolePayoutValidatorHash

              mTransactionInputs = map snd $ lookup marloweParams allContracts
            in
              case mUuid, mTransactionInputs of
                Just uuid, Just transactionInputs -> Just $ PlutusAppId uuid /\ { chParams: Just $ marloweParams /\ marloweData, chHistory: transactionInputs }
                _, _ -> Nothing
        pure $ Right $ fromFoldable $ values $ mapMaybeWithKey roleContractsToHistory roleContracts
  subscribeToPlutusApp dataProvider plutusAppId = Websocket.subscribeToContract $ toBack plutusAppId
  subscribeToWallet dataProvider wallet = Websocket.subscribeToWallet $ toBack wallet
  unsubscribeFromPlutusApp dataProvider plutusAppId = Websocket.unsubscribeFromContract $ toBack plutusAppId
  unsubscribeFromWallet dataProvider wallet = Websocket.unsubscribeFromWallet $ toBack wallet

instance monadMarloweHalogenM :: (ManageMarlowe m, ManageWebsocket m) => ManageMarlowe (HalogenM state action slots Msg m) where
  createWallet = lift createWallet
  followContract walletDetails marloweParams = lift $ followContract walletDetails marloweParams
  createPendingFollowerApp = lift <<< createPendingFollowerApp
  followContractWithPendingFollowerApp walletDetails marloweParams followAppId = lift $ followContractWithPendingFollowerApp walletDetails marloweParams followAppId
  createContract walletDetails roles contract = lift $ createContract walletDetails roles contract
  applyTransactionInput walletDetails marloweParams transactionInput = lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName = lift $ redeem walletDetails marloweParams tokenName
  lookupWalletInfo = lift <<< lookupWalletInfo
  lookupWalletDetails = lift <<< lookupWalletDetails
  getRoleContracts = lift <<< getRoleContracts
  getFollowerApps = lift <<< getFollowerApps
  subscribeToPlutusApp dataProvider plutusAppId = when (dataProvider /= LocalStorage) $ Websocket.subscribeToContract $ toBack plutusAppId
  subscribeToWallet dataProvider wallet = when (dataProvider /= LocalStorage) $ Websocket.subscribeToWallet $ toBack wallet
  unsubscribeFromPlutusApp dataProvider plutusAppId = when (dataProvider /= LocalStorage) $ Websocket.unsubscribeFromContract $ toBack plutusAppId
  unsubscribeFromWallet dataProvider wallet = when (dataProvider /= LocalStorage) $ Websocket.unsubscribeFromWallet $ toBack wallet
