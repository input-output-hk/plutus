module View.Contracts where

import Prelude hiding (div)
import Bootstrap (btn, btnBlock, btnPrimary, btnSmall, cardBody_, cardFooter_, cardHeader_, card_, col10_, col2_, col4_, nbsp, row_, tableBordered)
import Bootstrap as Bootstrap
import Clipboard (showShortCopyLong)
--import Data.Array (mapWithIndex, null)
import Data.Array (null)
import Data.Array as Array
import Data.Foldable.Extra (interleave)
import Data.Lens (_1, view)
import Data.Map as Map
import Data.Maybe (Maybe(..))
--import Data.Tuple.Nested (type (/\), (/\))
import Data.Tuple.Nested (type (/\))
import Halogen.HTML (ClassName(..), HTML, br_, button, div, div_, h2_, h3_, table, text, th, thead_, tr_)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, colSpan, disabled)
import Icons (Icon(..), icon)
--import Plutus.Contract.Resumable (IterationID(..), Request(..), RequestID(..))
import Network.StreamData as Stream
import Playground.Lenses (_endpointDescription, _getEndpointDescription, _schema)
import Playground.Types (_FunctionSchema)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.Contract.Effects (PABReq)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Schema.Types (FormEvent)
import Schema.View (actionArgumentForm)
import Types (ContractStates, EndpointForm, HAction(..), WebStreamData, _hooks)
import Validation (_argument)
--import View.Pretty (pretty)
--import View.Utils (webStreamDataPane)
import Wallet.Types (ContractInstanceId)

installedContractsPane ::
  forall p a.
  Show a =>
  Boolean ->
  Array (Builtin a) ->
  HTML p (HAction a)
installedContractsPane buttonsDisabled installedContracts =
  card_
    [ cardHeader_
        [ h2_ [ text "Installed Contracts" ]
        ]
    , cardBody_
        [ if null installedContracts then
            text "You do not have any contracts installed."
          else
            div_ (interleave br_ (installedContractPane buttonsDisabled <$> installedContracts))
        ]
    ]

installedContractPane ::
  forall p a.
  Show a =>
  Boolean ->
  Builtin a ->
  HTML p (HAction a)
installedContractPane buttonsDisabled installedContract =
  row_
    [ col2_
        [ button
            [ classes [ btn, btnSmall, btnPrimary, btnBlock ]
            , onClick (const $ Just $ ActivateContract installedContract)
            , disabled buttonsDisabled
            ]
            [ if buttonsDisabled then icon Spinner else text "Activate" ]
        ]
    , col10_ [ text $ show installedContract ]
    ]

contractStatusesPane ::
  forall p a.
  ContractStates ->
  HTML p (HAction a)
contractStatusesPane contractStates =
  card_
    [ cardHeader_
        [ h2_ [ text "Active Contracts" ]
        ]
    , cardBody_
        [ if null contractsWithRequests then
            text "You do not have any active contracts."
          else
            div_ (contractStatusPane <$> contractsWithRequests)
        ]
    ]
  where
  contractsWithRequests :: Array (WebStreamData (PartiallyDecodedResponse PABReq /\ Array EndpointForm))
  contractsWithRequests = Array.filter hasActiveRequests $ Array.fromFoldable $ Map.values contractStates

  hasActiveRequests :: WebStreamData (PartiallyDecodedResponse PABReq /\ Array EndpointForm) -> Boolean
  hasActiveRequests contractInstance = not $ null $ view (Stream._Success <<< _1 <<< _hooks) contractInstance

contractStatusPane ::
  forall p a.
  WebStreamData (PartiallyDecodedResponse PABReq /\ Array EndpointForm) ->
  HTML p (HAction a)
contractStatusPane contractState = div [ class_ $ ClassName "contract-status" ] []

-- $ webStreamDataPane
--    ( \(contractInstance /\ endpointForms) ->
--        let
--          contractInstanceId :: ContractInstanceId
--          contractInstanceId = view _csContract contractInstance
--        in
--          div_
--            [ contractRequestView contractInstance
--            , div_
--                [ row_
--                    ( mapWithIndex
--                        (\index endpointForm -> actionCard contractInstanceId (ChangeContractEndpointCall contractInstanceId index) endpointForm)
--                        endpointForms
--                    )
--                ]
--            ]
--    )
--    contractState
contractRequestView :: forall p a. PartiallyDecodedResponse PABReq -> HTML p (HAction a)
contractRequestView contractInstance =
  table [ classes [ Bootstrap.table, tableBordered ] ]
    [ thead_
        [ tr_
            [ th [ colSpan 3 ]
                [ h3_
                    [ ClipboardAction
                        <$> showShortCopyLong "" --contractInstanceIdString
                            ( Just
                                [ text "" --pretty $ view (_csContractDefinition) contractInstance
                                , nbsp
                                , text "-"
                                , nbsp
                                , text "" --contractInstanceIdString
                                ]
                            )
                    ]
                ]
            ]
        , tr_
            [ th [ class_ $ ClassName "iteration-id" ] [ text "Iteration" ]
            , th [ class_ $ ClassName "request-id" ] [ text "Request", nbsp, text "ID" ]
            , th [ class_ $ ClassName "request" ] [ text "Request" ]
            ]
        ]
    --, tbody_ (requestRow <$> requests)
    ]

--where
--contractInstanceIdString = view (_csContract <<< _contractInstanceIdString) contractInstance
--requests = view (_csCurrentState <<< _hooks) contractInstance
--requestRow (Request { itID: IterationID itID, rqID: RequestID rqID, rqRequest }) =
--  tr_
--    [ td_ [ text $ show itID ]
--    , td_ [ text $ show rqID ]
--    , td_ [ pretty rqRequest ]
--    ]
actionCard :: forall p a. ContractInstanceId -> (FormEvent -> (HAction a)) -> EndpointForm -> HTML p (HAction a)
actionCard contractInstanceId wrapper endpointForm =
  col4_
    [ card_
        [ cardHeader_ [ h2_ [ text $ view (_schema <<< _FunctionSchema <<< _endpointDescription <<< _getEndpointDescription) endpointForm ] ]
        , cardBody_ [ actionArgumentForm 0 wrapper (view _argument endpointForm) ]
        , cardFooter_
            [ button
                [ classes [ btn, btnSmall, btnPrimary ]
                , onClick $ const $ Just $ InvokeContractEndpoint contractInstanceId endpointForm
                ]
                [ text "Submit" ]
            ]
        ]
    ]
