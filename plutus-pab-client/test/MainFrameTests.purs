module MainFrameTests
  ( all
  ) where

import Prelude
import Types (HAction(..), Query, State, _currentView)
import Animation (class MonadAnimate)
import Clipboard (class MonadClipboard)
import Control.Monad.Except.Trans (class MonadThrow)
import Control.Monad.RWS (RWSResult(..), RWST(..), runRWST)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Either (Either(..))
import Data.Lens (_1, appendModifying, view)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import MainFrame (handleQuery, handleAction)
import MainFrame as MainFrame
import MonadApp (class MonadApp)
import Network.RemoteData (RemoteData(..))
import Plutus.PAB.Webserver (SPParams_(..))
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Test.QuickCheck ((<?>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import ContractExample (ExampleContracts)

type World
  = { console :: Array String }

execMockApp :: forall m a. MonadThrow Error m => World -> Array (Either (Query a) (HAction ExampleContracts)) -> m (Tuple World (State ExampleContracts))
execMockApp world queries = do
  let
    initialState = MainFrame.initialState

    handle (Left query) = void $ handleQuery query

    handle (Right action) = handleAction action
  RWSResult state result writer <-
    runRWST
      (unwrap (traverse_ handle queries :: MockApp m Unit))
      (defaultSettings (SPParams_ { baseURL: "/" }))
      (Tuple world initialState)
  pure state

-- | A dummy implementation of `MonadApp`, for testing the main handleAction loop.
newtype MockApp m a
  = MockApp (RWST (SPSettings_ SPParams_) Unit (Tuple World (State ExampleContracts)) m a)

derive instance newtypeMockApp :: Newtype (MockApp m a) _

derive newtype instance functorMockApp :: Functor m => Functor (MockApp m)

derive newtype instance applicativeMockApp :: Monad m => Applicative (MockApp m)

derive newtype instance applyMockApp :: Bind m => Apply (MockApp m)

derive newtype instance bindMockApp :: Bind m => Bind (MockApp m)

derive newtype instance monadMockApp :: Monad m => Monad (MockApp m)

derive newtype instance monadTransMockApp :: MonadTrans MockApp

derive newtype instance monadAskMockApp :: Monad m => MonadAsk (SPSettings_ SPParams_) (MockApp m)

instance monadStateMockApp :: Monad m => MonadState (State ExampleContracts) (MockApp m) where
  state f =
    MockApp
      $ RWST \r (Tuple world appState) -> case f appState of
          (Tuple a appState') -> pure $ RWSResult (Tuple world appState') a unit

instance monadAppMockApp :: Monad m => MonadApp (MockApp m) where
  activateContract _ = pure unit
  invokeEndpoint _ _ _ = pure Loading
  getFullReport = pure Loading
  getContractSignature _ = pure Loading
  sendWebSocketMessage _ = pure unit
  log msg =
    wrap
      $ appendModifying
          (_1 <<< prop (SProxy :: SProxy "console"))
          [ msg ]

-- | The mock app makes no attempt to animate anything, and just calls the embedded `action`.
instance monadAnimateMockApp :: MonadAnimate (MockApp m) (State ExampleContracts) where
  animate toggle action = action

instance monadClipboardMockApp :: Monad m => MonadClipboard (MockApp m) where
  copy _ = pure unit

------------------------------------------------------------
mockWorld :: World
mockWorld = { console: [] }

all :: TestSuite
all =
  suite "MainFrame" do
    evalTests

evalTests :: TestSuite
evalTests =
  suite "handleAction" do
    test "ChangeView" do
      quickCheck \aView -> do
        let
          result = execMockApp mockWorld [ Right $ ChangeView aView ]
        case result of
          Right (Tuple _ finalState) -> (aView == view _currentView finalState) <?> "Unexpected final view."
          Left err -> false <?> show err
