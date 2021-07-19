{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Plutus.Contracts.Auction(
    AuctionState(..),
    AuctionInput(..),
    BuyerSchema,
    SellerSchema,
    AuctionParams(..),
    HighestBid(..),
    auctionBuyer,
    auctionSeller,
    AuctionOutput(..),
    AuctionError(..),
    ThreadToken,
    SM.getThreadToken
    ) where

import           Control.Lens                     (makeClassyPrisms)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default                     (Default (def))
import           Data.Monoid                      (Last (..))
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           GHC.Generics                     (Generic)
import           Ledger                           (Ada, POSIXTime, PubKeyHash, Value)
import qualified Ledger
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Interval                  as Interval
import qualified Ledger.TimeSlot                  as TimeSlot
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Tx                  (TypedScriptTxOut (..))
import           Plutus.Contract
import           Plutus.Contract.StateMachine     (State (..), StateMachine (..), StateMachineClient, ThreadToken, Void,
                                                   WaitingResult (..))
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contract.Util             (loopM)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                          as Haskell


-- | Definition of an auction
data AuctionParams
    = AuctionParams
        { apOwner   :: PubKeyHash -- ^ Current owner of the asset. This is where the proceeds of the auction will be sent.
        , apAsset   :: Value -- ^ The asset itself. This value is going to be locked by the auction script output.
        , apEndTime :: POSIXTime -- ^ When the time window for bidding ends.
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionParams


data HighestBid =
    HighestBid
        { highestBid    :: Ada
        , highestBidder :: PubKeyHash
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''HighestBid

-- | The states of the auction
data AuctionState
    = Ongoing HighestBid -- Bids can be submitted.
    | Finished HighestBid -- The auction is finished
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

-- | Observable state of the auction app
data AuctionOutput =
    AuctionOutput
        { auctionState       :: Last AuctionState
        , auctionThreadToken :: Last ThreadToken
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid AuctionOutput) instance (Haskell.Semigroup AuctionOutput)
deriving via (GenericSemigroupMonoid AuctionOutput) instance (Haskell.Monoid AuctionOutput)

auctionStateOut :: AuctionState -> AuctionOutput
auctionStateOut s = Haskell.mempty { auctionState = Last (Just s) }

threadTokenOut :: ThreadToken -> AuctionOutput
threadTokenOut t = Haskell.mempty { auctionThreadToken = Last (Just t) }

-- | Initial 'AuctionState'. In the beginning the highest bid is 0 and the
--   highest bidder is seller of the asset. So if nobody submits
--   any bids, the seller gets the asset back after the auction has ended.
initialState :: PubKeyHash -> AuctionState
initialState self = Ongoing HighestBid{highestBid = 0, highestBidder = self}

PlutusTx.unstableMakeIsData ''AuctionState

-- | Transition between auction states
data AuctionInput
    = Bid { newBid :: Ada, newBidder :: PubKeyHash } -- Increase the price
    | Payout
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionInput

type AuctionMachine = StateMachine AuctionState AuctionInput

{-# INLINABLE auctionTransition #-}
-- | The transitions of the auction state machine.
auctionTransition :: AuctionParams -> State AuctionState -> AuctionInput -> Maybe (TxConstraints Void Void, State AuctionState)
auctionTransition AuctionParams{apOwner, apAsset, apEndTime} State{stateData=oldState} input =
    case (oldState, input) of

        (Ongoing HighestBid{highestBid, highestBidder}, Bid{newBid, newBidder}) | newBid > highestBid -> -- if the new bid is higher,
            let constraints =
                    Constraints.mustPayToPubKey highestBidder (Ada.toValue highestBid) -- we pay back the previous highest bid
                    <> Constraints.mustValidateIn (Interval.to apEndTime) -- but only if we haven't gone past 'apEndTime'
                newState =
                    State
                        { stateData = Ongoing HighestBid{highestBid = newBid, highestBidder = newBidder}
                        , stateValue = apAsset <> Ada.toValue newBid -- and lock the new bid in the script output
                        }
            in Just (constraints, newState)

        (Ongoing h@HighestBid{highestBidder, highestBid}, Payout) ->
            let constraints =
                    Constraints.mustValidateIn (Interval.from (apEndTime + 1)) -- When the auction has ended,
                    <> Constraints.mustPayToPubKey apOwner (Ada.toValue highestBid) -- the owner receives the payment
                    <> Constraints.mustPayToPubKey highestBidder apAsset -- and the highest bidder the asset
                newState = State { stateData = Finished h, stateValue = mempty }
            in Just (constraints, newState)

        -- Any other combination of 'AuctionState' and 'AuctionInput' is disallowed.
        -- This rules out new bids that don't go over the current highest bid.
        _ -> Nothing


{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: (ThreadToken, AuctionParams) -> AuctionMachine
auctionStateMachine (threadToken, auctionParams) = SM.mkStateMachine (Just threadToken) (auctionTransition auctionParams) isFinal where
    isFinal Finished{} = True
    isFinal _          = False

{-# INLINABLE mkValidator #-}
mkValidator :: (ThreadToken, AuctionParams) -> Scripts.ValidatorType AuctionMachine
mkValidator = SM.mkValidator . auctionStateMachine

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: (ThreadToken, AuctionParams) -> Scripts.TypedValidator AuctionMachine
typedValidator = Scripts.mkTypedValidatorParam @AuctionMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

-- | The machine client of the auction state machine. It contains the script instance
--   with the on-chain code, and the Haskell definition of the state machine for
--   off-chain use.
machineClient
    :: Scripts.TypedValidator AuctionMachine
    -> ThreadToken -- ^ Thread token of the instance
    -> AuctionParams
    -> StateMachineClient AuctionState AuctionInput
machineClient inst threadToken auctionParams =
    let machine = auctionStateMachine (threadToken, auctionParams)
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

type BuyerSchema = Endpoint "bid" Ada
type SellerSchema = EmptySchema -- Don't need any endpoints: the contract runs automatically until the auction is finished.

data AuctionLog =
    AuctionStarted AuctionParams
    | AuctionFailed SM.SMContractError
    | BidSubmitted HighestBid
    | AuctionEnded HighestBid
    | CurrentStateNotFound
    | TransitionFailed (SM.InvalidTransition AuctionState AuctionInput)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AuctionError =
    StateMachineContractError SM.SMContractError -- ^ State machine operation failed
    | AuctionContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''AuctionError

instance AsContractError AuctionError where
    _ContractError = _AuctionContractError . _ContractError

instance SM.AsSMContractError AuctionError where
    _SMContractError = _StateMachineContractError . SM._SMContractError

-- | Client code for the seller
auctionSeller :: Value -> POSIXTime -> Contract AuctionOutput SellerSchema AuctionError ()
auctionSeller value time = do
    threadToken <- SM.getThreadToken
    tell $ threadTokenOut threadToken
    self <- Ledger.pubKeyHash <$> ownPubKey
    let params       = AuctionParams{apOwner = self, apAsset = value, apEndTime = time }
        inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params

    _ <- handleError
            (\e -> do { logError (AuctionFailed e); throwError (StateMachineContractError e) })
            (SM.runInitialise client (initialState self) value)

    logInfo $ AuctionStarted params
    _ <- awaitTime time

    r <- SM.runStep client Payout
    case r of
        SM.TransitionFailure i            -> logError (TransitionFailed i) -- TODO: Add an endpoint "retry" to the seller?
        SM.TransitionSuccess (Finished h) -> logInfo $ AuctionEnded h
        SM.TransitionSuccess s            -> logWarn ("Unexpected state after Payout transition: " <> Haskell.show s)


-- | Get the current state of the contract and log it.
currentState :: StateMachineClient AuctionState AuctionInput -> Contract AuctionOutput BuyerSchema AuctionError (Maybe HighestBid)
currentState client = mapError StateMachineContractError (SM.getOnChainState client) >>= \case
    Just ((TypedScriptTxOut{tyTxOutData=Ongoing s}, _), _) -> do
        tell $ auctionStateOut $ Ongoing s
        pure (Just s)
    _ -> do
        logWarn CurrentStateNotFound
        pure Nothing

{- Note [Buyer client]

In the buyer client we want to keep track of the on-chain state of the auction
to give our user a chance to react if they are outbid by somebody else.

At the same time we want to have the "bid" endpoint active for any bids of our
own, and we want to stop the client when the auction is over.

To achieve this, we have a loop where we wait for one of several events to
happen and then deal with the event. The waiting is implemented in
@waitForChange@ and the event handling is in @handleEvent@.

Updates to the user are provided via 'tell'.

-}

data BuyerEvent =
        AuctionIsOver HighestBid -- ^ The auction has ended with the highest bid
        | SubmitOwnBid Ada -- ^ We want to submit a new bid
        | OtherBid HighestBid -- ^ Another buyer submitted a higher bid
        | NoChange HighestBid -- ^ Nothing has changed

waitForChange :: AuctionParams -> StateMachineClient AuctionState AuctionInput -> HighestBid -> Contract AuctionOutput BuyerSchema AuctionError (Waited BuyerEvent)
waitForChange AuctionParams{apEndTime} client lastHighestBid = do
    t <- currentTime
    let
        auctionOver = (AuctionIsOver lastHighestBid Haskell.<$) <$> awaitTime apEndTime
        submitOwnBid = endpoint @"bid" $ pure . SubmitOwnBid
        otherBid = do
            let address = Scripts.validatorAddress (SM.typedValidator (SM.scInstance client))
                targetTime = TimeSlot.slotToBeginPOSIXTime def
                           $ Haskell.succ
                           $ TimeSlot.posixTimeToEnclosingSlot def t
            bindWaited
                (addressChangeRequest
                    AddressChangeRequest
                    { acreqSlotRangeFrom = TimeSlot.posixTimeToEnclosingSlot def targetTime
                    , acreqSlotRangeTo = TimeSlot.posixTimeToEnclosingSlot def targetTime
                    , acreqAddress = address
                    })
                $ \AddressChangeResponse{acrTxns} ->
                    case acrTxns of
                        [] -> pure (NoChange lastHighestBid)
                        _  -> maybe (AuctionIsOver lastHighestBid) OtherBid <$> currentState client

    -- see note [Buyer client]
    auctionOver `select` submitOwnBid `select` otherBid

handleEvent :: StateMachineClient AuctionState AuctionInput -> HighestBid -> BuyerEvent -> Contract AuctionOutput BuyerSchema AuctionError (Either HighestBid ())
handleEvent client lastHighestBid change =
    let continue = pure . Left
        stop     = pure (Right ())
    -- see note [Buyer client]
    in case change of
        AuctionIsOver s -> tell (auctionStateOut $ Finished s) >> stop
        SubmitOwnBid ada -> do
            logInfo @Haskell.String "Submitting bid"
            self <- Ledger.pubKeyHash <$> ownPubKey
            logInfo @Haskell.String "Received pubkey"
            r <- SM.runStep client Bid{newBid = ada, newBidder = self}
            logInfo @Haskell.String "SM: runStep done"
            case r of
                SM.TransitionFailure i -> logError (TransitionFailed i) >> continue lastHighestBid
                SM.TransitionSuccess (Ongoing newHighestBid) -> logInfo (BidSubmitted newHighestBid) >> continue newHighestBid

                -- the last case shouldn't happen because the "Bid" transition always results in the "Ongoing"
                -- but you never know :-)
                SM.TransitionSuccess (Finished newHighestBid) -> logError (AuctionEnded newHighestBid) >> stop
        OtherBid s -> do
            tell (auctionStateOut $ Ongoing s)
            continue s
        NoChange s -> continue s

auctionBuyer :: ThreadToken -> AuctionParams -> Contract AuctionOutput BuyerSchema AuctionError ()
auctionBuyer currency params = do
    let inst   = typedValidator (currency, params)
        client = machineClient inst currency params

        -- the actual loop, see note [Buyer client]
        loop         = loopM (\h -> waitForChange params client h >>= handleEvent client h . getWaited)
    tell $ threadTokenOut currency
    initial <- currentState client
    case initial of
        Just s -> loop s

        -- If the state can't be found we wait for it to appear.
        Nothing -> getWaited <$> SM.waitForUpdateUntilTime client (apEndTime params) >>= \case
            WaitingResult (Ongoing s) -> loop s
            _                         -> logWarn CurrentStateNotFound
