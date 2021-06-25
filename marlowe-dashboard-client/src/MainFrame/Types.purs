module MainFrame.Types
  ( State
  , WebSocketStatus(..)
  , ChildSlots
  , Query(..)
  , Msg(..)
  , Action(..)
  ) where

import Prelude
import Analytics (class IsEvent, defaultEvent, toEvent)
import Contract.Types (State) as Contract
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Halogen as H
import Marlowe.PAB (PlutusAppId, CombinedWSStreamToServer)
import Marlowe.Semantics (Slot)
import Welcome.Types (Action, State) as Welcome
import Play.Types (Action, State) as Play
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient)
import Toast.Types (Action, State) as Toast
import Tooltip.Types (ReferenceId)
import WalletData.Types (WalletDetails, WalletLibrary)
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS
import WebSocket.Support (FromSocket) as WS

-- The app exists in one of two main subStates: the "welcome" state for when you have
-- no wallet, and all you can do is generate one or create a new one; and the "play"
-- state for when you have selected a wallet, and can do all of the things.
type State
  = { webSocketStatus :: WebSocketStatus
    , currentSlot :: Slot
    , subState :: Either Welcome.State Play.State
    , toast :: Toast.State
    }

data WebSocketStatus
  = WebSocketOpen
  | WebSocketClosed (Maybe WS.CloseEvent)

derive instance genericWebSocketStatus :: Generic WebSocketStatus _

instance showWebSocketStatus :: Show WebSocketStatus where
  show WebSocketOpen = "WebSocketOpen"
  show (WebSocketClosed Nothing) = "WebSocketClosed"
  show (WebSocketClosed (Just closeEvent)) = "WebSocketClosed " <> WS.reason closeEvent

------------------------------------------------------------
type ChildSlots
  = ( tooltipSlot :: forall query. H.Slot query Void ReferenceId
    )

------------------------------------------------------------
data Query a
  = ReceiveWebSocketMessage (WS.FromSocket CombinedWSStreamToClient) a
  | MainFrameActionQuery Action a

data Msg
  = SendWebSocketMessage CombinedWSStreamToServer
  | MainFrameActionMsg Action

------------------------------------------------------------
data Action
  = Init
  | EnterWelcomeState WalletLibrary WalletDetails (Map PlutusAppId Contract.State)
  | EnterPlayState WalletLibrary WalletDetails
  | WelcomeAction Welcome.Action
  | PlayAction Play.Action
  | ToastAction Toast.Action

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent Init = Just $ defaultEvent "Init"
  toEvent (EnterWelcomeState _ _ _) = Just $ defaultEvent "EnterWelcomeState"
  toEvent (EnterPlayState _ _) = Just $ defaultEvent "EnterPlayState"
  toEvent (WelcomeAction welcomeAction) = toEvent welcomeAction
  toEvent (PlayAction playAction) = toEvent playAction
  toEvent (ToastAction toastAction) = toEvent toastAction
