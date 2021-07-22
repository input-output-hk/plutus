{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
module Plutus.Contracts.Future(
    -- $future
      Future(..)
    , FutureAccounts(..)
    , mkAccounts
    , FutureError(..)
    , FutureSchema
    , FutureSetup(..)
    , Role(..)
    , futureContract
    , futureStateMachine
    , validator
    , initialiseFuture
    , initialMargin
    , futureAddress
    , tokenFor
    , initialState
    , typedValidator
    , setupTokens
    -- * Test data
    , testAccounts
    , setupTokensTrace
    ) where

import           Control.Lens                     (makeClassyPrisms, prism', review)
import           Control.Monad                    (void)
import           Control.Monad.Error.Lens         (throwing)
import qualified Control.Monad.Freer              as Freer
import qualified Control.Monad.Freer.Error        as Freer
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default                     (Default (..))
import           GHC.Generics                     (Generic)
import           Ledger                           (Address, Datum (..), POSIXTime, PubKey, Validator, ValidatorHash,
                                                   pubKeyHash)
import qualified Ledger
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Interval                  as Interval
import           Ledger.Oracle                    (Observation (..), SignedMessage (..))
import qualified Ledger.Oracle                    as Oracle
import           Ledger.Scripts                   (unitDatum)
import           Ledger.Tokens
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Plutus.Contract
import           Plutus.Contract.Util             (loopM)
import qualified PlutusTx
import           PlutusTx.Prelude

import           Plutus.Contract.StateMachine     (AsSMContractError, State (..), StateMachine (..), Void)
import qualified Plutus.Contract.StateMachine     as SM
import qualified Plutus.Contracts.Currency        as Currency
import           Plutus.Contracts.Escrow          (AsEscrowError (..), EscrowError, EscrowParams (..), RefundSuccess)
import qualified Plutus.Contracts.Escrow          as Escrow
import           Plutus.Contracts.TokenAccount    (Account (..))
import qualified Plutus.Contracts.TokenAccount    as TokenAccount
import qualified Plutus.Trace.Emulator            as Trace
import qualified Streaming.Prelude                as S
import qualified Wallet.Emulator.Folds            as Folds
import qualified Wallet.Emulator.Stream           as Stream
import qualified Wallet.Emulator.Wallet           as Wallet

import qualified Prelude                          as Haskell

-- $future
-- A futures contract in Plutus. This example illustrates a number of concepts.
--
--   1. Maintaining a margin (a kind of deposit) during the duration of the contract to protect against breach of contract (see note [Futures in Plutus])
--   2. Using oracle values to obtain current pricing information (see note [Oracles] in Plutus.Contracts)
--   3. Writing contracts as state machines
--   4. Using tokens to represent claims on future cash flows

-- | Basic data of a futures contract. `Future` contains all values that do not
--   change during the lifetime of the contract.
--
data Future =
    Future
        { ftDeliveryDate  :: POSIXTime
        , ftUnits         :: Integer
        , ftUnitPrice     :: Value
        , ftInitialMargin :: Value
        , ftPriceOracle   :: PubKey
        , ftMarginPenalty :: Value
        -- ^ How much a participant loses if they fail to make the required
        --   margin payments.
        } deriving Generic

-- | The two roles involved in the contract.
data Role = Long | Short
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

instance Eq Role where
    Long == Long   = True
    Short == Short = True
    _ == _         = False

-- | The token accounts that represent ownership of the two sides of the future.
--   When the contract is done, payments will be made to these accounts.
data FutureAccounts =
    FutureAccounts
        { ftoLong         :: Account
        -- ^ The owner of the "long" account (represented by a token)
        , ftoLongAccount  :: ValidatorHash
        -- ^ Address of the 'TokenAccount' validator script for 'ftoLong'. This --   hash can be derived from 'ftoLong', but only in off-chain code. We
        --   store it here so that we can lift it into on-chain code.
        , ftoShort        :: Account
        -- ^ The owner of the "short" account (represented by a token).
        , ftoShortAccount :: ValidatorHash
        -- ^ Address of the 'TokenAccount' validator script for 'ftoShort'. The
        --   comment on 'ftoLongAccount' applies to this as well.
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | The two margins.
data Margins =
    Margins
        { ftsShortMargin :: Value
        , ftsLongMargin  :: Value
        }
        deriving (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

instance Eq Margins where
    l == r = ftsShortMargin l == ftsShortMargin r && ftsLongMargin l == ftsLongMargin r

-- | The state of the future contract.
data FutureState =
    Running Margins
    -- ^ Ongoing contract, with the current margins.
    | Finished
    -- ^ Contract is finished.
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

instance Eq FutureState where
    Running ma == Running ma' = ma == ma'
    Finished   == Finished    = True
    _ == _                    = False

-- | Actions that can be performed on the future contract.
data FutureAction =
    AdjustMargin Role Value
    -- ^ Change the margin of one of the roles.
    | Settle (SignedMessage (Observation Value))
    -- ^ Close the contract at the delivery date by making the agreed payment
    --   and returning the margin deposits to their owners
    | SettleEarly (SignedMessage (Observation Value))
    -- ^ Close the contract early after a margin payment has been missed.
    --   The value of both margin accounts will be paid to the role that
    --   *didn't* violate the margin requirement
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data FutureError =
    TokenSetupFailed Currency.CurrencyError
    -- ^ Something went wrong during the setup of the two tokens
    | StateMachineError SM.SMContractError
    | OtherFutureError ContractError
    | EscrowFailed EscrowError
    -- ^ The escrow that initialises the future contract failed
    | EscrowRefunded RefundSuccess
    -- ^ The other party didn't make their payment in time so the contract never
    --   started.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''FutureError

instance AsSMContractError FutureError where
    _SMContractError = _StateMachineError

instance AsContractError FutureError where
    _ContractError = _OtherFutureError

instance AsCheckpointError FutureError where
    _CheckpointError = _OtherFutureError . _CheckpointError

type FutureSchema =
        Endpoint "initialise-future" (FutureSetup, Role)
        .\/ Endpoint "join-future" (FutureAccounts, FutureSetup)
        .\/ Endpoint "increase-margin" (Value, Role)
        .\/ Endpoint "settle-early" (SignedMessage (Observation Value))
        .\/ Endpoint "settle-future" (SignedMessage (Observation Value))

instance AsEscrowError FutureError where
    _EscrowError = prism' EscrowFailed (\case { EscrowFailed e -> Just e; _ -> Nothing})

futureContract :: Future -> Contract () FutureSchema FutureError ()
futureContract ft = do
    client <- joinFuture ft `select` initialiseFuture ft
    void $ loopM (const $ selectEither (increaseMargin client) (settleFuture client `select` settleEarly client)) ()

-- | The data needed to initialise the futures contract.
data FutureSetup =
    FutureSetup
        { shortPK       :: PubKey
        -- ^ Initial owner of the short token
        , longPK        :: PubKey
        -- ^ Initial owner of the long token
        , contractStart :: POSIXTime
        -- ^ Start of the futures contract itself. By this time the setup code
        --   has to be finished, otherwise the contract is void.
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

{- note [Futures in Plutus]

A futures contract ("future") is an agreement to change ownership of an
asset at a certain time (the delivery time) for an agreed price (the forward
price). The time of the transfer, and the price, are fixed at the beginning of
the contract.

A future can be settled either by actually exchanging the asset for the price
(physical settlement) or by exchanging the difference between the forward price
and the spot (current) price.

In Plutus we could do physical settlement for assets that exist on the
blockchain, that is, for tokens and currencies (everything that's a 'Value'). But
the contract implemented here is for cash settlement.

The agreement involves two parties, a buyer (long position) and a seller (short
position). At the delivery time the actual price of the asset (spot price) is
quite likely different from the forward price. If the spot price is higher than
the forward price, then the seller transfers the difference to the buyer. If
the spot price is lower than the forward price, then the buyer transfers money
to the seller. In either case there is a risk that the payer does not meet their
obligation (by simply not paying). To protect against this risk, the contract
includes a kind of deposit called "margin".

Each party deposits an initial margin. If the price moves against the seller,
then the seller has to top up their margin periodically (in our case, once each
block). Likewise, if it moves against the buyer then the buyer has to top up
their margin. If either party fails to make a margin payment then the contract
will be settled early.

The current value of the underlying asset is determined by an oracle. See note
[Oracles] in Plutus.Contracts. Also note that we
wouldn't need oracles if this was a contract with physical settlement,

The contract has three phases: Initialisation, runtime, and settlement. In the
first phase both parties deposit their initial margins into an escrow contract.
The second phase is when the contract is "live". In this phase the contract
is a state machine whose state is the 'MarginnAccounts' with the current margins.
The transition from the second to the third phase happens either after the
settlement date, or if the sport price has moved so far that one of the margin
accounts is underfunded. The runtime and settlement phases are modeled as a state
machine, with 'FutureState' and 'FutureAction' types.

-}


mkAccounts
    :: Account
    -> Account
    -> FutureAccounts
mkAccounts long short =
    FutureAccounts
        { ftoLong = long
        , ftoLongAccount = TokenAccount.validatorHash long
        , ftoShort = short
        , ftoShortAccount = TokenAccount.validatorHash short
        }

{-# INLINABLE tokenFor #-}
tokenFor :: Role -> FutureAccounts -> Value
tokenFor = \case
    Long  -> \case FutureAccounts{ftoLong=Account cur} -> token cur
    Short -> \case FutureAccounts{ftoShort=Account cur} -> token cur

{-# INLINABLE adjustMargin #-}
-- | Change the margin account of the role by the given amount.
adjustMargin :: Role -> Value -> Margins -> Margins
adjustMargin role value accounts =
    case role of
        Long  -> accounts { ftsLongMargin = ftsLongMargin accounts + value }
        Short -> accounts { ftsShortMargin = ftsShortMargin accounts + value }

{-# INLINABLE totalMargin #-}
-- | The combined value of both margin accounts.
totalMargin :: Margins -> Value
totalMargin Margins{ftsShortMargin, ftsLongMargin} =
    ftsShortMargin + ftsLongMargin

{-# INLINABLE futureStateMachine #-}
futureStateMachine
    :: Future
    -> FutureAccounts
    -> StateMachine FutureState FutureAction
futureStateMachine ft fos = SM.mkStateMachine Nothing (transition ft fos) isFinal where
    isFinal Finished = True
    isFinal _        = False

typedValidator :: Future -> FutureAccounts -> Scripts.TypedValidator (SM.StateMachine FutureState FutureAction)
typedValidator future ftos =
    let val = $$(PlutusTx.compile [|| validatorParam ||])
            `PlutusTx.applyCode`
                PlutusTx.liftCode future
                `PlutusTx.applyCode`
                    PlutusTx.liftCode ftos
        validatorParam f g = SM.mkValidator (futureStateMachine f g)
        wrap = Scripts.wrapValidator @FutureState @FutureAction

    in Scripts.mkTypedValidator @(SM.StateMachine FutureState FutureAction)
        val
        $$(PlutusTx.compile [|| wrap ||])

machineClient
    :: Scripts.TypedValidator (SM.StateMachine FutureState FutureAction)
    -> Future
    -> FutureAccounts
    -> SM.StateMachineClient FutureState FutureAction
machineClient inst future ftos =
    let machine = futureStateMachine future ftos
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

validator :: Future -> FutureAccounts -> Validator
validator ft fos = Scripts.validatorScript (typedValidator ft fos)

{-# INLINABLE verifyOracle #-}
verifyOracle :: PlutusTx.IsData a => PubKey -> SignedMessage a -> Maybe (a, TxConstraints Void Void)
verifyOracle pubKey sm =
    either (const Nothing) pure
    $ Oracle.verifySignedMessageConstraints pubKey sm

verifyOracleOffChain :: PlutusTx.IsData a => Future -> SignedMessage (Observation a) -> Maybe (POSIXTime, a)
verifyOracleOffChain Future{ftPriceOracle} sm =
    case Oracle.verifySignedMessageOffChain ftPriceOracle sm of
        Left _                               -> Nothing
        Right Observation{obsValue, obsTime} -> Just (obsTime, obsValue)

{-# INLINABLE transition #-}
transition :: Future -> FutureAccounts -> State FutureState -> FutureAction -> Maybe (TxConstraints Void Void, State FutureState)
transition future@Future{ftDeliveryDate, ftPriceOracle} owners State{stateData=s, stateValue=currentValue} i =
    case (s, i) of
        (Running accounts, AdjustMargin role topUp) ->
            Just ( mempty
                    , State
                    { stateData = Running (adjustMargin role topUp accounts)
                    , stateValue = topUp + totalMargin accounts
                    }
                    )
        (Running accounts, Settle ov)
            | Just (Observation{obsValue=spotPrice, obsTime=oracleDate}, oracleConstraints) <- verifyOracle ftPriceOracle ov, ftDeliveryDate == oracleDate ->
                let payment = payouts future accounts spotPrice
                    constraints =
                        Constraints.mustValidateIn (Interval.from ftDeliveryDate)
                        <> oracleConstraints
                        <> payoutsTx payment owners
                in Just ( constraints
                        , State
                            { stateData = Finished
                            , stateValue = mempty
                            }
                        )
        (Running accounts, SettleEarly ov)
            | Just (Observation{obsValue=spotPrice, obsTime=oracleDate}, oracleConstraints) <- verifyOracle ftPriceOracle ov, Just vRole <- violatingRole future accounts spotPrice, ftDeliveryDate > oracleDate ->
                let
                    total = totalMargin accounts
                    FutureAccounts{ftoLongAccount, ftoShortAccount} = owners
                    payment = case vRole of
                                Short -> Constraints.mustPayToOtherScript ftoLongAccount unitDatum total
                                Long  -> Constraints.mustPayToOtherScript ftoShortAccount unitDatum total
                    constraints = payment <> oracleConstraints
                in Just ( constraints
                        , State
                            { stateData = Finished
                            , stateValue = mempty
                            }
                        )
        _ -> Nothing

data Payouts =
    Payouts
        { payoutsShort :: Value
        , payoutsLong  :: Value
        }

{-# INLINABLE payoutsTx #-}
payoutsTx
    :: Payouts
    -> FutureAccounts
    -> TxConstraints Void Void
payoutsTx
    Payouts{payoutsShort, payoutsLong}
    FutureAccounts{ftoLongAccount, ftoShortAccount} =
        Constraints.mustPayToOtherScript ftoLongAccount unitDatum payoutsLong
        <> Constraints.mustPayToOtherScript ftoShortAccount unitDatum payoutsShort

{-# INLINABLE payouts #-}
-- | Compute the payouts for each role given the future data,
--   margin accounts, and current (spot) price
payouts :: Future -> Margins -> Value -> Payouts
payouts Future{ftUnits, ftUnitPrice} Margins{ftsShortMargin, ftsLongMargin} spotPrice =
    let delta = scale ftUnits (spotPrice - ftUnitPrice)
    in Payouts
        { payoutsShort = ftsShortMargin - delta
        , payoutsLong  = ftsLongMargin + delta
        }

-- | Compute the required margin from the current price of the
--   underlying asset.
{-# INLINABLE requiredMargin #-}
requiredMargin :: Future -> Value -> Value
requiredMargin Future{ftUnits, ftUnitPrice, ftMarginPenalty} spotPrice =
    let
        delta  = scale ftUnits (spotPrice - ftUnitPrice)
    in
        ftMarginPenalty + delta

{-# INLINABLE initialMargin #-}
initialMargin :: Future -> Value
initialMargin ft@Future{ftUnitPrice, ftMarginPenalty} =
    ftMarginPenalty + ftUnitPrice

{-# INLINABLE initialState #-}
-- | The initial state of the 'Future' contract
initialState :: Future -> FutureState
initialState ft =
    let im = initialMargin ft in
    Running (Margins{ftsShortMargin=im, ftsLongMargin=im})

futureAddress :: Future -> FutureAccounts -> Address
futureAddress ft fo = Ledger.scriptAddress (validator ft fo)

{-# INLINABLE violatingRole #-}
-- | The role that violated its margin requirements
violatingRole :: Future -> Margins -> Value -> Maybe Role
violatingRole future margins spotPrice =
    let
        minMargin = requiredMargin future spotPrice
        Margins{ftsShortMargin, ftsLongMargin} = margins
    in
    if ftsShortMargin `lt` minMargin then Just Short
    else if ftsLongMargin `lt` minMargin then Just Long
    else Nothing

-- | Initialise the contract by
--   * Generating the tokens for long and short
--   * Setting up an escrow contract for the initial margins
--   * Paying the initial margin for the given role
initialiseFuture
    :: ( HasEndpoint "initialise-future" (FutureSetup, Role) s
       , AsFutureError e
       )
    => Future
    -> Contract w s e (SM.StateMachineClient FutureState FutureAction)
initialiseFuture future = mapError (review _FutureError) $ do
    (s, ownRole) <- endpoint @"initialise-future" @(FutureSetup, Role)
    -- Start by setting up the two tokens for the short and long positions.
    ftos <- setupTokens

    -- Now we have a 'FutureAccountsValue' with the data of two new and unique
    -- tokens that we will use for the future contract. Now we use an escrow
    --  contract to initialise the future contract.

    inst <- checkpoint $ pure (typedValidator future ftos)

    let
        client = machineClient inst future ftos

        -- The escrow parameters 'esc' ensure that the initial margin is paid
        -- to the future contract address, and the two tokens are transferred
        -- to their initial owners.
        escr = escrowParams client future ftos s

        -- For the escrow to go through, both tokens and 2x the initial margin
        -- have to be deposited at the escrow address before the deadline
        -- (start of the future contract). Since we are currently in possession
        -- of both tokens, we pay the two tokens and our own initial margin to
        -- the escrow.
        payment =
            initialMargin future <> tokenFor Long ftos <> tokenFor Short ftos

        -- By using 'Escrow.payRedeemRefund' we make our payment and wait for
        -- the other party to make theirs. If they fail to do so within the
        -- agreed timeframe, our own initial margin is refunded and the future
        -- contract never starts.
        escrowPayment = Escrow.payRedeemRefund escr payment

    -- Run 'escrowPayment', wrapping any errors in 'EscrowFailed'. If the escrow
    -- contract ended with a refund (ie., 'escrowPayment' returns a 'Left') we
    -- throw an 'EscrowRefunded' error. If the escrow contract succeeded, the
    -- future is initialised and ready to go, so we return the 'FutureAccounts'
    -- with the token information.
    e <- mapError (review _EscrowFailed) escrowPayment
    either (throwing _EscrowRefunded) (\_ -> pure client) e

-- | The @"settle-future"@ endpoint. Given an oracle value with the current spot
--   price, this endpoint creates the final transaction that distributes the
--   funds locked by the future to the token accounts specified in
--   the 'FutureAccounts' argument.
settleFuture
    :: ( HasEndpoint "settle-future" (SignedMessage (Observation Value)) s
       , AsFutureError e
       )
    => SM.StateMachineClient FutureState FutureAction
    -> Contract w s e ()
settleFuture client = mapError (review _FutureError) $ do
    ov <- endpoint @"settle-future"
    void $ SM.runStep client (Settle ov)

-- | The @"settle-early"@ endpoint. Given an oracle value with the current spot
--   price, this endpoint creates the final transaction that distributes the
--   funds locked by the future to the token account of the role that did not
--   violate its obligations. Throws a 'MarginRequirementsNotViolated' error if
--   the spot price is within the margin range.
settleEarly
    :: ( HasEndpoint "settle-early" (SignedMessage (Observation Value)) s
       , AsSMContractError e
       , AsContractError e
       )
    => SM.StateMachineClient FutureState FutureAction
    -> Contract w s e ()
settleEarly client = do
    ov <- endpoint @"settle-early"
    void $ SM.runStep client (SettleEarly ov)

-- | The @"increase-margin"@ endpoint. Increses the margin of one of
--   the roles by an amount.
increaseMargin
    :: ( HasEndpoint "increase-margin" (Value, Role) s
       , AsSMContractError e
       , AsContractError e
       )
    => SM.StateMachineClient FutureState FutureAction
    -> Contract w s e ()
increaseMargin client = do
    (value, role) <- endpoint @"increase-margin"
    void $ SM.runStep client (AdjustMargin role value)

-- | The @"join-future"@ endpoint. Join a future contract by paying the initial
--   margin to the escrow that initialises the contract.
joinFuture
    :: ( HasEndpoint "join-future" (FutureAccounts, FutureSetup) s
       , AsFutureError e
       )
    => Future
    -> Contract w s e (SM.StateMachineClient FutureState FutureAction)
joinFuture ft = mapError (review _FutureError) $ do
    (owners, stp) <- endpoint @"join-future" @(FutureAccounts, FutureSetup)
    inst <- checkpoint $ pure (typedValidator ft owners)
    let client = machineClient inst ft owners
        escr = escrowParams client ft owners stp
        payment = Escrow.pay (Escrow.typedValidator escr) escr (initialMargin ft)
    void $ mapError EscrowFailed payment
    pure client

-- | Create two unique tokens that can be used for the short and long positions
--   and return a 'FutureAccounts' value for them.
--
--   Note that after 'setupTokens' is complete, both tokens will be locked by a
--   public key output belonging to the wallet that ran 'setupTokens'.
setupTokens
    :: forall w s e.
    ( AsFutureError e
    )
    => Contract w s e FutureAccounts
setupTokens = mapError (review _FutureError) $ do
    pk <- ownPubKey

    -- Create the tokens using the currency contract, wrapping any errors in
    -- 'TokenSetupFailed'
    cur <- mapError TokenSetupFailed $ Currency.mintContract (pubKeyHash pk) [("long", 1), ("short", 1)]
    let acc = Account . Value.assetClass (Currency.currencySymbol cur)
    pure $ mkAccounts (acc "long") (acc "short")

-- | The escrow contract that initialises the future. Both parties have to pay
--   their initial margin to this contract in order to unlock their tokens.
escrowParams
    :: SM.StateMachineClient FutureState FutureAction
    -> Future
    -> FutureAccounts
    -> FutureSetup
    -> EscrowParams Datum
escrowParams client future ftos FutureSetup{longPK, shortPK, contractStart} =
    let
        address = Ledger.validatorHash $ Scripts.validatorScript $ SM.typedValidator $ SM.scInstance client
        dataScript  = Ledger.Datum $ PlutusTx.toBuiltinData $ initialState future
        targets =
            [ Escrow.payToScriptTarget address
                dataScript
                (scale 2 (initialMargin future))
            , Escrow.payToPubKeyTarget (pubKeyHash longPK) (tokenFor Long ftos)
            , Escrow.payToPubKeyTarget (pubKeyHash shortPK) (tokenFor Short ftos)
            ]
    in EscrowParams
        { escrowDeadline = contractStart
        , escrowTargets = targets
        }

testAccounts :: FutureAccounts
testAccounts =
    let con = setupTokens @() @FutureSchema @FutureError
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag (Wallet.Wallet 1))
        getOutcome (Folds.Done a) = a
        getOutcome e              = Haskell.error $ "not finished: " <> Haskell.show e
    in
    either (Haskell.error . Haskell.show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream def setupTokensTrace

setupTokensTrace :: Trace.EmulatorTrace ()
setupTokensTrace = do
    _ <- Trace.waitNSlots 1
    _ <- Trace.activateContractWallet (Wallet.Wallet 1) (void $ setupTokens @() @FutureSchema @FutureError)
    void $ Trace.waitNSlots 2

PlutusTx.makeLift ''Future
PlutusTx.makeLift ''FutureAccounts
PlutusTx.makeLift ''Margins
PlutusTx.unstableMakeIsData ''Margins
PlutusTx.makeLift ''Role
PlutusTx.unstableMakeIsData ''Role
PlutusTx.makeLift ''FutureState
PlutusTx.unstableMakeIsData ''FutureState
PlutusTx.makeLift ''FutureAction
PlutusTx.unstableMakeIsData ''FutureAction
