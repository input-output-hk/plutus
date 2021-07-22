module View (render) where

import Prelude hiding (div)
import Bootstrap (col12_, col5_, container_, row_)
import Cardano.Metadata.Types (Property)
import Cardano.Metadata.Types as Metadata
import Chain.Types as Chain
import Data.Array as Array
import Data.Foldable (findMap)
import Data.Lens (traversed, view)
import Data.Lens.Extra (toArrayOf)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, div, div_, h1, text)
import Halogen.HTML.Properties (class_, classes)
import Icons (Icon(..), icon)
import NavTabs (mainTabBar, viewContainer)
--import Network.RemoteData as RemoteData
import Network.StreamData as Stream
import Plutus.PAB.Webserver.Types (ChainReport)
import Types (ContractSignatures, ContractStates, HAction(..), State(..), View(..), WebSocketStatus(..), WebStreamData, _csrDefinition, _utxoIndex, _unContractSignatures)
import View.Blockchain (annotatedBlockchainPane)
import View.Contracts (contractStatusesPane, installedContractsPane)
import View.Events (utxoIndexPane)
--import View.Events (eventsPane, utxoIndexPane)
import View.Utils (streamErrorPane, webDataPane, webStreamDataPane)

render ::
  forall m slots a.
  MonadAff m =>
  Show a =>
  State a ->
  ComponentHTML (HAction a) slots m
render (State { currentView, chainState, contractSignatures, chainReport, contractStates, webSocketStatus, webSocketMessage, metadata }) =
  div
    [ class_ $ ClassName "main-frame" ]
    [ container_
        [ mainHeader
        , mainTabBar ChangeView tabs currentView
        , webSocketStatusIcon webSocketStatus
        , div_
            $ case webSocketMessage of
                Stream.Failure error -> [ streamErrorPane error ]
                _ -> []
        , div_
            $ webDataPane (mainPane currentView contractStates chainState contractSignatures) chainReport
        -- $ webDataPane2 (mainPane currentView contractStates chainState contractSignatures) chainReport events
        ]
    ]

nameIfAvailable ::
  forall k.
  Map Metadata.Subject (Map k Property) -> String -> String
nameIfAvailable metadata key =
  fromMaybe key
    $ do
        properties <- Map.lookup (Metadata.Subject key) metadata
        findMap
          ( case _ of
              Metadata.Name name _ -> Just name
              _ -> Nothing
          )
          (Array.fromFoldable (Map.values properties))

mainHeader :: forall p a. HTML p (HAction a)
mainHeader =
  div_
    [ h1
        [ class_ $ ClassName "main-title" ]
        [ text "Plutus Application Platform" ]
    ]

tabs :: Array { help :: String, link :: View, title :: String }
tabs =
  [ { link: ActiveContracts
    , title: "Contracts"
    , help: "See available and active contracts."
    }
  , { link: Blockchain
    , title: "Blockchain"
    , help: "See the current state of the chain."
    }
  , { link: EventLog
    , title: "Event Log"
    , help: "View the history of system events."
    }
  ]

webSocketStatusIcon :: forall p i. WebSocketStatus -> HTML p i
webSocketStatusIcon webSocketStatus =
  div
    [ classes
        [ webSocketStatusClass
        , webSocketStatusClass
            <> ClassName
                ( case webSocketStatus of
                    WebSocketOpen -> "-open"
                    (WebSocketClosed _) -> "-closed"
                )
        ]
    ]
    [ icon
        $ case webSocketStatus of
            WebSocketOpen -> CheckCircle
            (WebSocketClosed _) -> ExclamationCircle
    ]
  where
  webSocketStatusClass = ClassName "web-socket-status"

mainPane ::
  forall p a.
  Show a =>
  View ->
  ContractStates ->
  Chain.State ->
  WebStreamData (ContractSignatures a) ->
  ChainReport ->
  --Array (ChainEvent ContractExe) ->
  HTML p (HAction a)
mainPane currentView contractStates chainState contractSignatures chainReport {-events-} =
  row_
    [ activeContractPane currentView contractSignatures contractStates
    , blockchainPane currentView chainState chainReport
    --, eventLogPane currentView events chainReport
    ]

activeContractPane ::
  forall p a.
  Show a =>
  View ->
  WebStreamData (ContractSignatures a) ->
  ContractStates ->
  HTML p (HAction a)
activeContractPane currentView contractSignatures contractStates =
  let
    buttonsDisabled = Stream.isExpected contractSignatures
  in
    viewContainer currentView ActiveContracts
      [ row_
          [ col12_ [ contractStatusesPane contractStates ]
          , col12_
              ( webStreamDataPane
                  ( \x ->
                      installedContractsPane buttonsDisabled
                        ( ( toArrayOf
                              ( traversed
                                  <<< _csrDefinition
                              )
                          )
                            (view _unContractSignatures x)
                        )
                  )
                  contractSignatures
              )
          ]
      ]

blockchainPane ::
  forall p a.
  View ->
  Chain.State ->
  ChainReport -> HTML p (HAction a)
blockchainPane currentView chainState chainReport =
  viewContainer currentView Blockchain
    [ row_
        [ col12_ [ ChainAction <$> annotatedBlockchainPane chainState chainReport ]
        ]
    ]

eventLogPane ::
  forall p a.
  View ->
  --Array (ChainEvent ContractExe) ->
  ChainReport ->
  HTML p (HAction a)
eventLogPane currentView {-events-} chainReport =
  viewContainer currentView EventLog
    [ row_
        --[ col7_ [ eventsPane events ]
        [ col5_ [ utxoIndexPane (view _utxoIndex chainReport) ]
        ]
    ]
