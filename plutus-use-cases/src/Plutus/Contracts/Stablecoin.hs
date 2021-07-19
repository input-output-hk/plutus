{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- Stablecoin backed by a cryptocurrency.

= Concept

The bank keeps a reserve of an underlying base cryptocurrency. The bank
issues and redeems stablecoins and reservecoins.

The stablecoin is pegged to a peg currency, for example USD or BTC. The
exchange rate between the peg currency and the base cryptocurrency is provided
by an oracle.

The holder of a stablecoin has a claim to a variable portion of the reserves
according to the exchange rate

The bank's equity is the bank's reserves minus the bank's liabilities to the
stablecoin holders. The bank's equity is owned by the reservecoin holders.

The bank profits (and its reserve grows) by minting and redeeming the
stablecoins and reservecoins for a fee.

The bank's reserve ratio is the bank's reserves divided by the amount of
stablecoins in circulation. The bank is only allowed to issue and sell
stablecoins and reservecoins if the reserve ratio remains above a minimum
threshold. The minimum threshold aims to ensure that stablecoins remain fully
backed by reserves even if the price of the base currency falls.

= Implementation

The contract is written as a state machine defined in the 'step' function. It
supports two operations: mint a number of stablecoins and mint a number of
reservecoins. Stablecoins and reservecoins can be redeemed by minting a
negative number of them.

An oracle value with the current exchange rate of the base currency has to be
provided with every transition.

The two coins (stablecoin and reservecoin) are Plutus native token whose minting
policy is the forwarding policy for the stablecoin's state machine.

We use the 'Ratio' type for all calculations in the script, using 'round' to
obtain 'Integer' values at the very end.

-}
module Plutus.Contracts.Stablecoin(
  SC(..)
  , RC(..)
  , BC(..)
  , PC(..)
  , BankState(..)
  , Stablecoin(..)
  , Input(..)
  , SCAction(..)
  , ConversionRate
  -- * State machine client
  , typedValidator
  , machineClient
  , step
  -- * Contract using the state machine
  , contract
  , StablecoinError
  , StablecoinSchema
  -- * Etc.
  , stableCoins
  , reserveCoins
  , checkValidState
  ) where

import           Control.Lens                 (makeClassyPrisms)
import           Control.Monad                (forever, guard)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Functor.Identity        (Identity (..))
import           GHC.Generics                 (Generic)
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Constraints           as Constraints
import           Ledger.Crypto                (PubKey)
import qualified Ledger.Interval              as Interval
import           Ledger.Oracle
import           Ledger.Scripts               (MintingPolicyHash)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import           Ledger.Value                 (AssetClass, TokenName, Value)
import qualified Ledger.Value                 as Value
import           Plutus.Contract
import           Plutus.Contract.StateMachine (AsSMContractError, SMContractError, State (..), StateMachine,
                                               StateMachineClient (..), Void)
import qualified Plutus.Contract.StateMachine as StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.Ratio               as R
import qualified Prelude                      as Haskell

-- | Conversion rate from peg currency (eg. USD) to base currency (eg. Ada)
type ConversionRate = Ratio Integer

-- Amounts of stablecoins and reservecoins (used for bookkeeping)
-- SC, RC and BC are values that can be represented on-chain with the 'Value'
-- type. PC is a currency such as USD that exists outside the chain, so we
-- will never have a 'Value' containing PC.

-- | An amount of stablecoins
newtype SC a = SC { unSC :: a }
    deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving Functor via Identity

-- | An amount of reservecoins
newtype RC a = RC { unRC :: a }
    deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving Functor via Identity

-- | An amount of base currency coins (eg. Ada or some native currency)
newtype BC a = BC { unBC :: a }
    deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving Functor via Identity

-- | An amount of peg currency (eg. USD)
newtype PC a = PC { unPC :: a }
    deriving newtype (Haskell.Num, Eq, Ord, AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)
    deriving Functor via Identity

-- | The bank's state
data BankState =
    BankState
        { bsReserves            :: BC Integer -- ^ Value of the bank's reserves in base currency
        , bsStablecoins         :: SC Integer -- ^ Amount of stablecoins in circulation
        , bsReservecoins        :: RC Integer -- ^ Amount of reservecoins currently in circulation
        , bsMintingPolicyScript :: MintingPolicyHash -- ^ Hash of the minting policy that forwards all checks to the state machine. (This has to be in this type, rather than in 'Stablecoin', to avoid a circular dependency on the script's hash)
        }
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

-- | Initialise the 'BankState' with zero deposits.
initialState :: StateMachineClient BankState Input -> BankState
initialState StateMachineClient{scInstance=StateMachine.StateMachineInstance{StateMachine.typedValidator}} =
    BankState
        { bsReserves = 0
        , bsStablecoins = 0
        , bsReservecoins = 0
        , bsMintingPolicyScript = Scripts.forwardingMintingPolicyHash typedValidator
        }

{-# INLINEABLE convert #-}
-- | Convert peg currency units to base currency units using the
--   observed conversion rate
convert :: ConversionRate -> PC (Ratio Integer) -> BC (Ratio Integer)
convert rate (PC pc) =
    BC $ rate * pc

{-# INLINEABLE liabilities #-}
-- | The bank's liabilities (total value of stablecoins in base currency)
liabilities ::
    BankState
    -> ConversionRate
    -> BC (Ratio Integer)
liabilities BankState{bsReserves=BC reserves,bsStablecoins=SC stablecoins} cr =
    let BC stableCoinLiabilities = convert cr (PC $ fromInteger stablecoins)
    in BC (min (fromInteger reserves) stableCoinLiabilities)

{-# INLINEABLE equity #-}
-- | The bank's equity (what's left of the reserves after subtracting
--   liabilities).
equity ::
    BankState
    -> ConversionRate
    -> BC (Ratio Integer)
equity r@BankState{bsReserves=BC reserves} cr =
    let BC l = liabilities r cr
    in BC (fromInteger reserves - l)

-- | Stablecoin parameters.
data Stablecoin =
    Stablecoin
        { scOracle                  :: PubKey -- ^ Public key of the oracle that provides exchange rates
        , scFee                     :: Ratio Integer -- ^ Fee charged by bank for transactions. Calculated as a fraction of the total transaction volume in base currency.
        , scMinReserveRatio         :: Ratio Integer -- ^ The minimum ratio of reserves to liabilities
        , scMaxReserveRatio         :: Ratio Integer -- ^ The maximum ratio of reserves to liabilities
        , scReservecoinDefaultPrice :: BC Integer -- ^ The price of a single reservecoin if no reservecoins have been issued
        , scBaseCurrency            :: AssetClass -- ^ The asset class of the base currency. Value of this currency will be locked by the stablecoin state machine instance
        , scStablecoinTokenName     :: TokenName -- ^ 'TokenName' of the stablecoin
        , scReservecoinTokenName    :: TokenName -- ^ 'TokenName' of the reservecoin
        }
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

{-# INLINEABLE minReserve #-}
-- | Minimum number of base currency coins held by the bank.
--   Returns 'Nothing' if no stablecoins have been minted.
minReserve :: Stablecoin -> ConversionRate -> BankState -> Maybe (BC (Ratio Integer))
minReserve Stablecoin{scMinReserveRatio} cr BankState{bsStablecoins=SC sc}
    | sc == zero = Nothing
    | otherwise =
        let BC r = convert cr (PC $ fromInteger sc)
        in Just $ BC $ scMinReserveRatio * r

-- | Maximum number of base currency coins held by the bank.
--   Returns 'Nothing' if no stablecoins have been minted.
{-# INLINEABLE maxReserve #-}
maxReserve :: Stablecoin -> ConversionRate -> BankState -> Maybe (BC (Ratio Integer))
maxReserve Stablecoin{scMaxReserveRatio} cr BankState{bsStablecoins=SC sc}
    | sc == zero = Nothing
    | otherwise =
        let BC r = convert cr (PC $ fromInteger sc)
        in Just $ BC $ scMaxReserveRatio * r

{-# INLINEABLE reservecoinNominalPrice #-}
-- | Price of a single reservecoin in base currency
reservecoinNominalPrice :: Stablecoin -> BankState -> ConversionRate -> BC (Ratio Integer)
reservecoinNominalPrice Stablecoin{scReservecoinDefaultPrice} bankState@BankState{bsReservecoins=RC rc} cr
    | rc /= 0 = let BC e = equity bankState cr in BC (e * R.recip (fromInteger rc))
    | otherwise = fmap fromInteger scReservecoinDefaultPrice

{-# INLINEABLE stablecoinNominalPrice #-}
-- | Price of a single stablecoin in base currency. If the banks' liabilities
--   exceed its reserves then 'stablecoinNominalPrice' is zero.
stablecoinNominalPrice :: BankState -> ConversionRate -> BC (Ratio Integer)
stablecoinNominalPrice bankState@BankState{bsStablecoins=SC sc} cr
    | sc == zero = BC p
    | otherwise  = BC $ min p l
    where
        BC p = convert cr (PC $ fromInteger 1)
        BC l = liabilities bankState cr

-- | Action that can be performed on the stablecoin contract.
data SCAction
    = MintStablecoin (SC Integer) -- ^ Create a number stablecoins, depositing the matching amount of base currency
    | MintReserveCoin (RC Integer) -- ^ Create a number of reservecoins, depositing the matching amount of base currency
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

{-# INLINEABLE calcFees #-}
-- | Calculate transaction fees (paid in base currency to the bank) as a
--   fraction of the transaction's volume
calcFees :: Stablecoin -> BankState -> ConversionRate -> SCAction -> BC (Ratio Integer)
calcFees sc@Stablecoin{scFee} bs conversionRate = \case
    MintStablecoin (SC i) ->
        stablecoinNominalPrice bs conversionRate * BC scFee * (BC $ abs $ fromInteger i)
    MintReserveCoin (RC i) ->
        reservecoinNominalPrice sc bs conversionRate * BC scFee * (BC $ abs $ fromInteger i)

-- | Input to the stablecoin state machine
data Input =
    Input
        { inpSCAction       :: SCAction -- ^ The action to be performed
        , inpConversionRate :: SignedMessage (Observation ConversionRate) -- ^ Exchange rate between base currency and peg currency
        }
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

{-# INLINEABLE bankReservesValue #-}
-- | The 'Value' containing the bank's reserve in base currency. This is
--   the 'Value' locked by the state machine output with that state.
bankReservesValue :: Stablecoin -> BankState -> Value
bankReservesValue Stablecoin{scBaseCurrency} BankState{bsReserves = BC i} =
    Value.assetClassValue scBaseCurrency i

{-# INLINEABLE transition #-}
transition :: Stablecoin -> State BankState -> Input -> Maybe (TxConstraints Void Void, State BankState)
transition sc State{stateData=oldState} input =
    let toSmState state = State{stateData=state, stateValue=bankReservesValue sc state}
    in fmap (\(constraints, newState) -> (constraints, toSmState newState)) (step sc oldState input)

{-# INLINEABLE applyInput #-}
-- | Given a stablecoin definition, current state and input, compute the
--   new state and tx constraints, without checking whether the new state
--   is valid.
applyInput :: forall i o. Stablecoin -> BankState -> Input -> Maybe (TxConstraints i o, BankState)
applyInput sc@Stablecoin{scOracle,scStablecoinTokenName,scReservecoinTokenName} bs@BankState{bsMintingPolicyScript} Input{inpSCAction, inpConversionRate} = do
    (Observation{obsValue=rate, obsTime}, constraints) <- either (const Nothing) pure (verifySignedMessageConstraints scOracle inpConversionRate)
    let fees = calcFees sc bs rate inpSCAction
        (newState, newConstraints) = case inpSCAction of
            MintStablecoin sc' ->
                let scValue = stablecoinNominalPrice bs rate * (BC $ fromInteger $ unSC sc') in
                (bs
                { bsStablecoins = bsStablecoins bs + sc'
                , bsReserves = bsReserves bs + fmap round (fees + scValue)
                }, Constraints.mustMintCurrency bsMintingPolicyScript scStablecoinTokenName (unSC sc'))
            MintReserveCoin rc ->
                let rcValue = reservecoinNominalPrice sc bs rate * (BC $ fromInteger $ unRC rc) in
                (bs
                { bsReservecoins = bsReservecoins bs + rc
                , bsReserves = bsReserves bs + fmap round (fees + rcValue)
                }, Constraints.mustMintCurrency bsMintingPolicyScript scReservecoinTokenName (unRC rc))
    let dateConstraints = Constraints.mustValidateIn $ Interval.from obsTime
    pure (constraints <> newConstraints <> dateConstraints, newState)

{-# INLINEABLE step #-}
step :: forall i o. Stablecoin -> BankState -> Input -> Maybe (TxConstraints i o, BankState)
step sc@Stablecoin{scOracle} bs i@Input{inpConversionRate} = do
    (constraints, newState) <- applyInput sc bs i
    (Observation{obsValue=rate}, _ :: TxConstraints i o) <- either (const Nothing) pure (verifySignedMessageConstraints scOracle inpConversionRate)
    guard $ isValidState sc newState rate
    pure (constraints, newState)

-- | A 'Value' with the given number of reservecoins
reserveCoins :: Stablecoin -> RC Integer -> Value
reserveCoins sc@Stablecoin{scReservecoinTokenName} =
    let sym = Scripts.forwardingMintingPolicyHash $ typedValidator sc
    in Value.singleton (Value.mpsSymbol sym) scReservecoinTokenName . unRC

-- | A 'Value' with the given number of stablecoins
stableCoins :: Stablecoin -> SC Integer -> Value
stableCoins sc@Stablecoin{scStablecoinTokenName} =
    let sym = Scripts.forwardingMintingPolicyHash $ typedValidator sc
    in Value.singleton (Value.mpsSymbol sym) scStablecoinTokenName . unSC

{-# INLINEABLE isValidState #-}
isValidState :: Stablecoin -> BankState -> ConversionRate -> Bool
isValidState sc bs cr = isRight (checkValidState sc bs cr)

{-# INLINEABLE checkValidState #-}
checkValidState :: Stablecoin -> BankState -> ConversionRate -> Either InvalidStateReason ()
checkValidState sc bs@BankState{bsReservecoins, bsReserves, bsStablecoins} cr = do
    -- TODO: Do we need a validation type in the state machine lib?
    unless (bsReservecoins >= RC 0) (Left NegativeReserveCoins)
    unless (bsReserves >= BC 0) (Left NegativeReserves)
    unless (bsStablecoins >= SC 0) (Left NegativeStablecoins)
    unless (liabilities bs cr >= zero) (Left NegativeLiabilities)
    unless (equity bs cr >= zero) (Left NegativeEquity)

    let actualReserves = fmap fromInteger bsReserves
        allowedReserves = (,) <$> minReserve sc cr bs <*> maxReserve sc cr bs

    case allowedReserves of
        Just (minReserves, maxReserves) -> do
            unless (actualReserves >= minReserves) (Left $ MinReserves minReserves actualReserves)
            unless (actualReserves <= maxReserves) (Left $ MaxReserves maxReserves actualReserves)
        Nothing -> pure ()

data InvalidStateReason
    = NegativeReserveCoins
    | NegativeReserves
    | NegativeStablecoins
    | MinReserves { allowed :: BC (Ratio Integer), actual :: BC (Ratio Integer)  }
    | MaxReserves { allowed :: BC (Ratio Integer), actual :: BC (Ratio Integer)  }
    | NegativeLiabilities
    | NegativeEquity
    deriving (Haskell.Show)

stablecoinStateMachine :: Stablecoin -> StateMachine BankState Input
stablecoinStateMachine sc = StateMachine.mkStateMachine Nothing (transition sc) isFinal
    -- the state machine never stops (OK for the prototype but we probably need
    -- to add a final state to the real thing)
    where isFinal _ = False

typedValidator :: Stablecoin -> Scripts.TypedValidator (StateMachine BankState Input)
typedValidator stablecoin =
    let val = $$(PlutusTx.compile [|| validator ||]) `PlutusTx.applyCode` PlutusTx.liftCode stablecoin
        validator d = StateMachine.mkValidator (stablecoinStateMachine d)
        wrap = Scripts.wrapValidator @BankState @Input
    in Scripts.mkTypedValidator @(StateMachine BankState Input) val $$(PlutusTx.compile [|| wrap ||])

machineClient ::
    Scripts.TypedValidator (StateMachine BankState Input)
    -> Stablecoin
    -> StateMachineClient BankState Input
machineClient inst stablecoin =
    let machine = stablecoinStateMachine stablecoin
    in StateMachine.mkStateMachineClient (StateMachine.StateMachineInstance machine inst)

type StablecoinSchema =
        Endpoint "run step" Input
        .\/ Endpoint "initialise" Stablecoin

data StablecoinError =
    InitialiseEPError ContractError
    | StateMachineError SMContractError
    | RunStepError ContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''StablecoinError

instance AsContractError StablecoinError where
    _ContractError = _InitialiseEPError . _ContractError

instance AsSMContractError StablecoinError where
    _SMContractError = _StateMachineError . StateMachine._SMContractError

-- | A 'Contract' that initialises the state machine and then accepts 'Input'
--   transitions.
contract :: Contract () StablecoinSchema StablecoinError (Waited ())
contract = endpoint @"initialise" $ \sc -> do
    let theClient = machineClient (typedValidator sc) sc
    _ <- StateMachine.runInitialise theClient (initialState theClient) mempty
    forever $ endpoint @"run step" $ \i -> do
        checkTransition theClient sc i
        StateMachine.runStep theClient i

-- | Apply 'checkValidState' to the states before and after a transition
--   and log a warning if something isn't right.
checkTransition :: StateMachineClient BankState Input -> Stablecoin -> Input -> Contract () StablecoinSchema StablecoinError ()
checkTransition theClient sc i@Input{inpConversionRate} = do
        currentState <- mapError StateMachineError $ StateMachine.getOnChainState theClient
        case checkHashOffChain inpConversionRate of
            Right Observation{obsValue} -> do
                case currentState of
                    Just ((TypedScriptTxOut{tyTxOutData}, _), _) -> do
                        case checkValidState sc tyTxOutData obsValue of
                            Right _ -> logInfo @Haskell.String "Current state OK"
                            Left w  -> logInfo $ "Current state is invalid: " <> Haskell.show w <> ". The transition may still be allowed."
                        case applyInput sc tyTxOutData i of
                            Just (_, newState) -> case checkValidState sc newState obsValue of
                                Right _ -> logInfo @Haskell.String "New state OK"
                                Left w  -> logWarn $ "New state is invalid: " <> Haskell.show w <> ". The transition is not allowed."
                            Nothing -> logWarn @Haskell.String "applyInput is Nothing (transition failed)"
                    Nothing -> logWarn @Haskell.String "Unable to find current state."
            Left e -> logWarn $ "Unable to decode oracle value from datum: " <> Haskell.show e

PlutusTx.makeLift ''SC
PlutusTx.makeLift ''RC
PlutusTx.makeLift ''BC
PlutusTx.makeLift ''PC
PlutusTx.makeLift ''BankState
PlutusTx.makeLift ''Stablecoin
PlutusTx.unstableMakeIsData ''BC
PlutusTx.unstableMakeIsData ''SC
PlutusTx.unstableMakeIsData ''RC
PlutusTx.unstableMakeIsData ''PC
PlutusTx.unstableMakeIsData ''BankState
PlutusTx.unstableMakeIsData ''Stablecoin
PlutusTx.unstableMakeIsData ''SCAction
PlutusTx.unstableMakeIsData ''Input
