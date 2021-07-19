module Types where

import Prelude
import Cardano.Metadata.Types (Property(..), PropertyKey(..))
import Cardano.Metadata.Types as Metadata
import Chain.Types as Chain
import Clipboard as Clipboard
import Control.Monad.Gen as Gen
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Json.JsonUUID (JsonUUID, _JsonUUID)
import Data.Json.JsonTuple (JsonTuple)
import Data.Lens (Getter', Iso', Traversal', Lens', to, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\))
import Data.UUID as UUID
import Foreign (MultipleErrors)
import Plutus.Contract.Resumable (Request)
import Ledger.Index (UtxoIndex)
import Plutus.V1.Ledger.Tx (Tx)
import Plutus.V1.Ledger.TxId (TxId)
import Network.RemoteData (RemoteData)
import Network.StreamData (StreamData)
import Network.StreamData as Stream
import Playground.Types (FunctionSchema)
import Plutus.Contract.Effects (PABReq, ActiveEndpoint, _ExposeEndpointReq)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Webserver.Types (ChainReport, ContractReport, ContractSignatureResponse, _ChainReport, _ContractReport, _ContractSignatureResponse, CombinedWSStreamToClient, CombinedWSStreamToServer)
import Schema (FormSchema)
import Schema.Types (FormArgument, FormEvent)
import Servant.PureScript.Ajax (AjaxError)
import Test.QuickCheck (class Arbitrary)
import Wallet.Rollup.Types (AnnotatedTx)
import Wallet.Types (ContractInstanceId, EndpointDescription)
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS
import WebSocket.Support (FromSocket) as WS

data Query a
  = ReceiveWebSocketMessage (WS.FromSocket CombinedWSStreamToClient) a

data Output
  = SendWebSocketMessage CombinedWSStreamToServer

data StreamError
  = DecodingError MultipleErrors
  | ServerError String
  | TransportError AjaxError

type WebStreamData
  = StreamData StreamError

type WebData
  = RemoteData AjaxError

fromWebData :: forall a. WebData a -> WebStreamData a
fromWebData = Stream.fromRemoteData <<< lmap TransportError

data HAction a
  = Init
  | ChangeView View
  | LoadFullReport
  | ActivateContract (Builtin a)
  | ChainAction Chain.Action
  | ClipboardAction Clipboard.Action
  | ChangeContractEndpointCall ContractInstanceId Int FormEvent
  | InvokeContractEndpoint ContractInstanceId EndpointForm

type ContractStates
  = Map ContractInstanceId (WebStreamData (PartiallyDecodedResponse PABReq /\ Array EndpointForm))

newtype ContractSignatures a
  = ContractSignatures
  { unContractSignatures :: Array (ContractSignatureResponse (Builtin a))
  }

derive instance genericContractSignatures :: Generic (ContractSignatures a) _

derive instance newtypeContractSignatures :: Newtype (ContractSignatures a) _

_ContractSignatures :: forall a. Iso' (ContractSignatures a) { unContractSignatures :: Array (ContractSignatureResponse (Builtin a)) }
_ContractSignatures = _Newtype

data WebSocketStatus
  = WebSocketOpen
  | WebSocketClosed (Maybe WS.CloseEvent)

derive instance genericWebSocketStatus :: Generic WebSocketStatus _

instance showWebSocketStatus :: Show WebSocketStatus where
  show WebSocketOpen = "WebSocketOpen"
  show (WebSocketClosed Nothing) = "WebSocketClosed"
  show (WebSocketClosed (Just closeEvent)) = "WebSocketClosed " <> WS.reason closeEvent

newtype State a
  = State
  { currentView :: View
  , contractSignatures :: WebStreamData (ContractSignatures a)
  , chainReport :: WebData ChainReport
  , chainState :: Chain.State
  , contractStates :: ContractStates
  , webSocketMessage :: WebStreamData CombinedWSStreamToClient
  , webSocketStatus :: WebSocketStatus
  , metadata :: Map Metadata.Subject (Map PropertyKey Property)
  }

type EndpointForm
  = { schema :: FunctionSchema FormSchema
    , argument :: FormArgument
    }

derive instance newtypeState :: Newtype (State a) _

derive instance genericState :: Generic (State a) _

_currentView :: forall a. Lens' (State a) View
_currentView = _Newtype <<< prop (SProxy :: SProxy "currentView")

_contractSignatures :: forall s r a. Newtype s { contractSignatures :: a | r } => Lens' s a
_contractSignatures = _Newtype <<< prop (SProxy :: SProxy "contractSignatures")

_chainReport :: forall s r a. Newtype s { chainReport :: a | r } => Lens' s a
_chainReport = _Newtype <<< prop (SProxy :: SProxy "chainReport")

_events :: forall s r a. Newtype s { events :: a | r } => Lens' s a
_events = _Newtype <<< prop (SProxy :: SProxy "events")

_chainState :: forall a. Lens' (State a) Chain.State
_chainState = _Newtype <<< prop (SProxy :: SProxy "chainState")

_contractStates :: forall a. Lens' (State a) ContractStates
_contractStates = _Newtype <<< prop (SProxy :: SProxy "contractStates")

_metadata ::
  forall a.
  Lens' (State a)
    ( Map Metadata.Subject (Map PropertyKey Property)
    )
_metadata = _Newtype <<< prop (SProxy :: SProxy "metadata")

_annotatedBlockchain :: Lens' ChainReport (Array (Array AnnotatedTx))
_annotatedBlockchain = _ChainReport <<< prop (SProxy :: SProxy "annotatedBlockchain")

_transactionMap :: Lens' ChainReport (Map TxId Tx)
_transactionMap = _ChainReport <<< prop (SProxy :: SProxy "transactionMap")

_webSocketMessage :: forall s a r. Newtype s { webSocketMessage :: a | r } => Lens' s a
_webSocketMessage = _Newtype <<< prop (SProxy :: SProxy "webSocketMessage")

_webSocketStatus :: forall s a r. Newtype s { webSocketStatus :: a | r } => Lens' s a
_webSocketStatus = _Newtype <<< prop (SProxy :: SProxy "webSocketStatus")

_contractReport :: forall s a r. Newtype s { contractReport :: a | r } => Lens' s a
_contractReport = _Newtype <<< prop (SProxy :: SProxy "contractReport")

_utxoIndex :: Lens' ChainReport UtxoIndex
_utxoIndex = _ChainReport <<< prop (SProxy :: SProxy "utxoIndex")

_crAvailableContracts :: forall t. Lens' (ContractReport t) (Array (ContractSignatureResponse t))
_crAvailableContracts = _ContractReport <<< prop (SProxy :: SProxy "crAvailableContracts")

_crActiveContractStates :: forall t. Lens' (ContractReport t) (Array (JsonTuple ContractInstanceId (PartiallyDecodedResponse PABReq)))
_crActiveContractStates = _ContractReport <<< prop (SProxy :: SProxy "crActiveContractStates")

_csrDefinition :: forall t. Lens' (ContractSignatureResponse t) t
_csrDefinition = _ContractSignatureResponse <<< prop (SProxy :: SProxy "csrDefinition")

_unContractSignatures :: forall t. Lens' (ContractSignatures t) (Array (ContractSignatureResponse (Builtin t)))
_unContractSignatures = _ContractSignatures <<< prop (SProxy :: SProxy "unContractSignatures")

-- _csContract :: forall t. Lens' (ContractInstanceState t) ContractInstanceId
-- _csContract = _Newtype <<< prop (SProxy :: SProxy "csContract")
-- _csCurrentState :: forall t. Lens' (ContractInstanceState t) (PartiallyDecodedResponse PABReq)
-- _csCurrentState = _Newtype <<< prop (SProxy :: SProxy "csCurrentState")
-- _csContractDefinition :: forall t. Lens' (ContractInstanceState t) t
-- _csContractDefinition = _ContractInstanceState <<< prop (SProxy :: SProxy "csContractDefinition")
_hooks :: forall t. Lens' (PartiallyDecodedResponse t) (Array (Request t))
_hooks = _Newtype <<< prop (SProxy :: SProxy "hooks")

_activeEndpoint :: Lens' ActiveEndpoint EndpointDescription
_activeEndpoint = _Newtype <<< prop (SProxy :: SProxy "aeDescription")

_contractActiveEndpoints :: Traversal' (PartiallyDecodedResponse PABReq) EndpointDescription
_contractActiveEndpoints =
  _hooks
    <<< traversed
    <<< _rqRequest
    <<< _ExposeEndpointReq
    <<< _activeEndpoint

_rqRequest :: forall t. Lens' (Request t) t
_rqRequest = _Newtype <<< prop (SProxy :: SProxy "rqRequest")

_contractInstanceId :: Lens' ContractInstanceId JsonUUID
_contractInstanceId = _Newtype <<< prop (SProxy :: SProxy "unContractInstanceId")

_contractInstanceIdString :: Getter' ContractInstanceId String
_contractInstanceIdString = _contractInstanceId <<< _JsonUUID <<< to UUID.toString

------------------------------------------------------------
data View
  = ActiveContracts
  | Blockchain
  | EventLog

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (ActiveContracts :| [ Blockchain, EventLog ])

instance showView :: Show View where
  show = genericShow

------------------------------------------------------------
toPropertyKey :: Property -> PropertyKey
toPropertyKey (Preimage _ _) = PropertyKey "preimage"

toPropertyKey (Name _ _) = PropertyKey "name"

toPropertyKey (Description _ _) = PropertyKey "description"

toPropertyKey (Other name _ _) = PropertyKey name

_getPubKeyHash :: forall s r a. Newtype s { getPubKeyHash :: a | r } => Lens' s a
_getPubKeyHash = _Newtype <<< prop (SProxy :: SProxy "getPubKeyHash")

propertyName :: Metadata.Property -> (Maybe String)
propertyName (Metadata.Name name _) = Just name

propertyName _ = Nothing
