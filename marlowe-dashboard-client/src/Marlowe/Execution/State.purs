module Marlowe.Execution.State
  ( mkInitialState
  , nextState
  , nextTimeout
  , mkTx
  , timeoutState
  , isClosed
  , getActionParticipant
  , extractNamedActions
  , expandBalances
  , getAllPayments
  ) where

import Prelude
import Data.Array as Array
import Data.BigInteger (fromInt)
import Data.Lens (view, (^.))
import Data.List (List, concat, fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Tuple.Nested ((/\))
import Marlowe.Execution.Lenses (_resultingPayments)
import Marlowe.Execution.Types (NamedAction(..), PendingTimeouts, State)
import Marlowe.Semantics (Accounts, Action(..), Case(..), ChoiceId(..), Contract(..), Input, Party, Payment, ReduceResult(..), Slot(..), SlotInterval(..), Timeouts(..), Token, TransactionInput(..), TransactionOutput(..), _accounts, _boundValues, _minSlot, computeTransaction, emptyState, evalValue, makeEnvironment, reduceContractUntilQuiescent, timeouts)
import Marlowe.Semantics (State) as Semantic

mkInitialState :: Slot -> Contract -> State
mkInitialState currentSlot contract =
  { semanticState: emptyState currentSlot
  , contract
  , history: mempty
  , mPendingTimeouts: Nothing
  , mNextTimeout: nextTimeout contract
  }

-- FIXME: Change the order of the arguments to::  TransactionInput -> State  -> State
nextState :: State -> TransactionInput -> State
nextState { semanticState, contract, history } txInput =
  let
    TransactionInput { interval: SlotInterval minSlot maxSlot } = txInput

    { txOutState, txOutContract, txOutPayments } = case computeTransaction txInput semanticState contract of
      (TransactionOutput { txOutState, txOutContract, txOutPayments }) -> { txOutState, txOutContract, txOutPayments }
      -- We should not have contracts which cause errors in the dashboard so we will just ignore error cases for now
      -- FIXME: Change nextState to return an Either
      -- TODO: SCP-2088 We need to discuss how to display the warnings that computeTransaction may give
      (Error _) -> { txOutState: semanticState, txOutContract: contract, txOutPayments: mempty }
  in
    { semanticState: txOutState
    , contract: txOutContract
    , history: Array.snoc history { initialSemanticState: semanticState, txInput, resultingPayments: txOutPayments }
    , mPendingTimeouts: Nothing
    , mNextTimeout: nextTimeout txOutContract
    }

nextTimeout :: Contract -> Maybe Slot
nextTimeout = timeouts >>> \(Timeouts { minTime }) -> minTime

mkTx :: Slot -> Contract -> List Input -> TransactionInput
mkTx currentSlot contract inputs =
  let
    interval = mkInterval currentSlot contract
  in
    TransactionInput { interval, inputs }

timeoutState :: Slot -> State -> State
timeoutState currentSlot { semanticState, contract, history, mPendingTimeouts, mNextTimeout } =
  let
    Slot slot = currentSlot

    env = makeEnvironment slot slot

    advanceAllTimeouts ::
      Maybe Slot ->
      Array Slot ->
      Semantic.State ->
      Contract ->
      { mNextTimeout :: Maybe Slot, mPendingTimeouts :: Maybe PendingTimeouts }
    advanceAllTimeouts mNextTimeout' newTimeouts state' contract'
      | mNextTimeout' /= Nothing && mNextTimeout' <= Just currentSlot =
        let
          { txOutState, txOutContract } = case reduceContractUntilQuiescent env state' contract' of
            -- TODO: SCP-2088 We need to discuss how to display the warnings that computeTransaction may give
            ContractQuiescent _ _ _ txOutState txOutContract -> { txOutState, txOutContract }
            -- FIXME: Change timeoutState to return an Either
            RRAmbiguousSlotIntervalError -> { txOutState: state', txOutContract: contract' }

          newNextTimeout = nextTimeout txOutContract
        in
          advanceAllTimeouts newNextTimeout (Array.snoc newTimeouts currentSlot) txOutState txOutContract
      | otherwise =
        { mNextTimeout: mNextTimeout'
        , mPendingTimeouts:
            if newTimeouts == mempty then
              Nothing
            else
              Just
                { nextSemanticState: state'
                , continuationContract: contract'
                , timeouts: newTimeouts
                }
        }

    { nextSemanticState, continuationContract, timeouts } =
      fromMaybe'
        ( \_ ->
            { nextSemanticState: semanticState, continuationContract: contract, timeouts: [] }
        )
        mPendingTimeouts

    advancedTimeouts = advanceAllTimeouts mNextTimeout timeouts nextSemanticState continuationContract
  in
    { semanticState
    , contract
    , history
    , mPendingTimeouts: advancedTimeouts.mPendingTimeouts
    , mNextTimeout: advancedTimeouts.mNextTimeout
    }

------------------------------------------------------------
isClosed :: State -> Boolean
isClosed { contract: Close } = true

isClosed _ = false

getActionParticipant :: NamedAction -> Maybe Party
getActionParticipant (MakeDeposit _ party _ _) = Just party

getActionParticipant (MakeChoice (ChoiceId _ party) _ _) = Just party

getActionParticipant _ = Nothing

extractNamedActions :: Slot -> State -> Array NamedAction
extractNamedActions _ { mPendingTimeouts: Just { continuationContract: Close } } = [ CloseContract ]

extractNamedActions currentSlot { mPendingTimeouts: Just { nextSemanticState, continuationContract } } = extractActionsFromContract currentSlot nextSemanticState continuationContract

extractNamedActions currentSlot { semanticState, contract } = extractActionsFromContract currentSlot semanticState contract

-- a When can only progress if it has timed out or has Cases
extractActionsFromContract :: Slot -> Semantic.State -> Contract -> Array NamedAction
extractActionsFromContract _ _ Close = mempty

extractActionsFromContract currentSlot semanticState contract@(When cases timeout cont)
  -- in the case of a timeout we need to provide an Evaluate action to all users to "manually" progress the contract
  | currentSlot > timeout =
    let
      minSlot = view _minSlot semanticState

      emptyTx = TransactionInput { interval: SlotInterval minSlot minSlot, inputs: mempty }

      outputs = case computeTransaction emptyTx semanticState cont of
        TransactionOutput { txOutPayments, txOutState } ->
          let
            oldBindings = view _boundValues semanticState

            newBindings = view _boundValues txOutState

            bindings = Map.difference newBindings oldBindings
          in
            { payments: Array.fromFoldable txOutPayments, bindings: bindings }
        _ -> mempty
    in
      -- FIXME: Currently we don't have a way to display Evaluate so this can be dangerous.
      --        This should not happen for the demo as we are storing pending timeouts with
      --        the latest contract that hasn't timeouted.
      [ Evaluate outputs ]
  -- if there are no cases then there is no action that any user can take to progress the contract
  | otherwise = cases <#> \(Case action _) -> toNamedAction action
    where
    toNamedAction (Deposit a p t v) =
      let
        SlotInterval (Slot minSlot) (Slot maxSlot) = mkInterval currentSlot contract

        env = makeEnvironment minSlot maxSlot

        amount = evalValue env semanticState v
      in
        MakeDeposit a p t amount

    toNamedAction (Choice cid bounds) = MakeChoice cid bounds Nothing

    toNamedAction (Notify obs) = MakeNotify obs

-- In reality other situations should never occur as contracts always reduce to When or Close
-- however someone could in theory publish a contract that starts with another Contract constructor
-- and we would want to enable moving forward with Evaluate
extractActionsFromContract _ _ _ = [ Evaluate mempty ]

-- This function expands the balances inside the Semantic.State to all participants and tokens,
-- using zero if the participant does not have balance for that token.
expandBalances :: Array Party -> Array Token -> Semantic.State -> Accounts
expandBalances participants tokens semanticState =
  let
    stateAccounts = semanticState ^. _accounts
  in
    Map.fromFoldable do
      party <- participants
      tokens
        <#> \token ->
            let
              key = party /\ token
            in
              key /\ (fromMaybe zero $ Map.lookup key stateAccounts)

mkInterval :: Slot -> Contract -> SlotInterval
mkInterval currentSlot contract = case nextTimeout contract of
  Nothing -> SlotInterval currentSlot (currentSlot + (Slot $ fromInt 10))
  Just minTime
    -- FIXME: I think this will fail in the PAB... we may need to return a Maybe SlotInterval and
    -- show an error. But also, if you delay confirming the action it could also cause the same type
    -- of failure, so maybe there is no need. Check after initial PAB integration.
    | minTime < currentSlot -> SlotInterval currentSlot currentSlot
    | otherwise -> SlotInterval currentSlot (minTime - (Slot $ fromInt 1))

getAllPayments :: State -> List Payment
getAllPayments { history } = concat $ fromFoldable $ map (view _resultingPayments) history
