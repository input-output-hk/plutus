{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
-- Big hammer, but helps
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

{-| = Marlowe: financial contracts domain specific language for blockchain

Here we present a reference implementation of Marlowe, domain-specific language targeted at
the execution of financial contracts in the style of Peyton Jones et al
on Cardano.

== Semantics

Semantics is based on <https://github.com/input-output-hk/marlowe/blob/stable/src/Semantics.hs>

Marlowe Contract execution is a chain of transactions,
where remaining contract and its state is passed through /Datum/,
and actions (i.e. /Choices/) are passed as
/Redeemer Script/

/Validation Script/ is always the same Marlowe interpreter implementation, available below.
-}

module Language.Marlowe.Semantics where

import           Control.Applicative      ((<*>), (<|>))
import           Control.Newtype.Generics (Newtype)
import qualified Data.Aeson               as JSON
import qualified Data.Aeson.Extras        as JSON
import           Data.Aeson.Types         hiding (Error, Value)
import qualified Data.Foldable            as F
import           Data.Scientific          (Scientific, floatingOrInteger)
import           Data.Text                (pack)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Deriving.Aeson
import           Language.Marlowe.Pretty  (Pretty (..))
import           Ledger                   (PubKeyHash (..), Slot (..), ValidatorHash)
import           Ledger.Value             (CurrencySymbol (..), TokenName (..))
import qualified Ledger.Value             as Val
import           PlutusTx                 (makeIsDataIndexed)
import           PlutusTx.AssocMap        (Map)
import qualified PlutusTx.AssocMap        as Map
import           PlutusTx.Lift            (makeLift)
import           PlutusTx.Prelude         hiding (encodeUtf8, mapM, (<$>), (<*>), (<>))
import           PlutusTx.Ratio           (denominator, numerator)
import           Prelude                  (mapM, (<$>))
import qualified Prelude                  as Haskell
import           Text.PrettyPrint.Leijen  (comma, hang, lbrace, line, rbrace, space, text, (<>))

{- HLINT ignore "Avoid restricted function" -}

{- Functions that used in Plutus Core must be inlineable,
   so their code is available for PlutusTx compiler -}
{-# INLINABLE inBounds #-}
{-# INLINABLE fixInterval #-}
{-# INLINABLE evalValue #-}
{-# INLINABLE evalObservation #-}
{-# INLINABLE refundOne #-}
{-# INLINABLE moneyInAccount #-}
{-# INLINABLE updateMoneyInAccount #-}
{-# INLINABLE addMoneyToAccount #-}
{-# INLINABLE giveMoney #-}
{-# INLINABLE reduceContractStep #-}
{-# INLINABLE reduceContractUntilQuiescent #-}
{-# INLINABLE applyCases #-}
{-# INLINABLE applyInput #-}
{-# INLINABLE convertReduceWarnings #-}
{-# INLINABLE applyAllInputs #-}
{-# INLINABLE isClose #-}
{-# INLINABLE computeTransaction #-}
{-# INLINABLE contractLifespanUpperBound #-}
{-# INLINABLE totalBalance #-}

-- * Aliaces

data Party = PK PubKeyHash | Role TokenName
  deriving stock (Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)

instance Haskell.Show Party where
  showsPrec p (PK pk) = Haskell.showParen (p Haskell.>= 11) $ Haskell.showString "PK \""
                                              . Haskell.showsPrec 11 pk
                                              . Haskell.showString "\""
  showsPrec _ (Role role) = Haskell.showsPrec 11 $ unTokenName role

type AccountId = Party
type Timeout = Slot
type Money = Val.Value
type ChoiceName = ByteString
type ChosenNum = Integer
type SlotInterval = (Slot, Slot)
type Accounts = Map (AccountId, Token) Integer

-- * Data Types
{-| Choices – of integers – are identified by ChoiceId
    which combines a name for the choice with the Party who had made the choice.
-}
data ChoiceId = ChoiceId ByteString Party
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-| Token - represents a currency or token, it groups
    a pair of a currency symbol and token name.
-}
data Token = Token CurrencySymbol TokenName
  deriving stock (Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)

instance Haskell.Show Token where
  showsPrec p (Token cs tn) =
    Haskell.showParen (p Haskell.>= 11) (Haskell.showString $ "Token \"" Haskell.++ Haskell.show cs Haskell.++ "\" " Haskell.++ Haskell.show tn)

{-| Values, as defined using Let ar e identified by name,
    and can be used by 'UseValue' construct.
-}
newtype ValueId = ValueId ByteString
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
  deriving anyclass (Newtype)


{-| Values include some quantities that change with time,
    including “the slot interval”, “the current balance of an account (in Lovelace)”,
    and any choices that have already been made.

    Values can also be scaled, and combined using addition, subtraction, and negation.
-}
data Value a = AvailableMoney AccountId Token
           | Constant Integer
           | NegValue (Value a)
           | AddValue (Value a) (Value a)
           | SubValue (Value a) (Value a)
           | MulValue (Value a) (Value a)
           | Scale Rational (Value a)
           | ChoiceValue ChoiceId
           | SlotIntervalStart
           | SlotIntervalEnd
           | UseValue ValueId
           | Cond a (Value a) (Value a)
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-| Observations are Boolean values derived by comparing values,
    and can be combined using the standard Boolean operators.

    It is also possible to observe whether any choice has been made
    (for a particular identified choice).
-}
data Observation = AndObs Observation Observation
                 | OrObs Observation Observation
                 | NotObs Observation
                 | ChoseSomething ChoiceId
                 | ValueGE (Value Observation) (Value Observation)
                 | ValueGT (Value Observation) (Value Observation)
                 | ValueLT (Value Observation) (Value Observation)
                 | ValueLE (Value Observation) (Value Observation)
                 | ValueEQ (Value Observation) (Value Observation)
                 | TrueObs
                 | FalseObs
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


data Bound = Bound Integer Integer
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-| Actions happen at particular points during execution.
    Three kinds of action are possible:

    * A @Deposit n p v@ makes a deposit of value @v@ into account @n@ belonging to party @p@.

    * A choice is made for a particular id with a list of bounds on the values that are acceptable.
      For example, @[(0, 0), (3, 5]@ offers the choice of one of 0, 3, 4 and 5.

    * The contract is notified that a particular observation be made.
      Typically this would be done by one of the parties,
      or one of their wallets acting automatically.
-}
data Action = Deposit AccountId Party Token (Value Observation)
            | Choice ChoiceId [Bound]
            | Notify Observation
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-| A payment can be made to one of the parties to the contract,
    or to one of the accounts of the contract,
    and this is reflected in the definition.
-}
data Payee = Account AccountId
           | Party Party
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-  Plutus doesn't support mutually recursive data types yet.
    datatype Case is mutually recurvive with @Contract@
-}
data Case a = Case Action a
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-| Marlowe has six ways of building contracts.
    Five of these – 'Pay', 'Let', 'If', 'When' and 'Assert' –
    build a complex contract from simpler contracts, and the sixth, 'Close',
    is a simple contract.
    At each step of execution, as well as returning a new state and continuation contract,
    it is possible that effects – payments – and warnings can be generated too.
-}
data Contract = Close
              | Pay AccountId Payee Token (Value Observation) Contract
              | If Observation Contract Contract
              | When [Case Contract] Timeout Contract
              | Let ValueId (Value Observation) Contract
              | Assert Observation Contract
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)


{-| Marlowe contract internal state. Stored in a /Datum/ of a transaction output.
-}
data State = State { accounts    :: Accounts
                   , choices     :: Map ChoiceId ChosenNum
                   , boundValues :: Map ValueId Integer
                   , minSlot     :: Slot }
  deriving stock (Haskell.Show,Haskell.Eq,Generic)

{-| Execution environment. Contains a slot interval of a transaction.
-}
newtype Environment = Environment { slotInterval :: SlotInterval }
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord)


{-| Input for a Marlowe contract. Correspond to expected 'Action's.
-}
data Input = IDeposit AccountId Party Token Integer
           | IChoice ChoiceId ChosenNum
           | INotify
  deriving stock (Haskell.Show,Haskell.Eq,Generic)
  deriving anyclass (Pretty)

instance FromJSON Input where
  parseJSON (String "input_notify") = return INotify
  parseJSON (Object v) =
        (IDeposit <$> (v .: "into_account")
                  <*> (v .: "input_from_party")
                  <*> (v .: "of_token")
                  <*> (v .: "that_deposits"))
    <|> (IChoice <$> (v .: "for_choice_id")
                 <*> (v .: "input_that_chooses_num"))
  parseJSON _ = Haskell.fail "Contract must be either an object or a the string \"close\""

instance ToJSON Input where
  toJSON (IDeposit accId party tok amount) = object
      [ "input_from_party" .= party
      , "that_deposits" .= amount
      , "of_token" .= tok
      , "into_account" .= accId
      ]
  toJSON (IChoice choiceId chosenNum) = object
      [ "input_that_chooses_num" .= chosenNum
      , "for_choice_id" .= choiceId
      ]
  toJSON INotify = JSON.String $ pack "input_notify"


{-| Slot interval errors.
    'InvalidInterval' means @slotStart > slotEnd@, and
    'IntervalInPastError' means slot interval is in the past, relative to the contract.

    These errors should never occur, but we are always prepared.
-}
data IntervalError = InvalidInterval SlotInterval
                   | IntervalInPastError Slot SlotInterval
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)


-- | Result of 'fixInterval'
data IntervalResult = IntervalTrimmed Environment State
                    | IntervalError IntervalError
  deriving stock (Haskell.Show)


{-| Payment occurs during 'Pay' contract evaluation, and
    when positive balances are payed out on contract closure.
-}
data Payment = Payment AccountId Payee Money
  deriving stock (Haskell.Show)


-- | Effect of 'reduceContractStep' computation
data ReduceEffect = ReduceWithPayment Payment
                  | ReduceNoPayment
  deriving stock (Haskell.Show)


-- | Warning during 'reduceContractStep'
data ReduceWarning = ReduceNoWarning
                   | ReduceNonPositivePay AccountId Payee Token Integer
                   | ReducePartialPay AccountId Payee Token Integer Integer
--                                      ^ src    ^ dest       ^ paid ^ expected
                   | ReduceShadowing ValueId Integer Integer
--                                     oldVal ^  newVal ^
                   | ReduceAssertionFailed
  deriving stock (Haskell.Show)


-- | Result of 'reduceContractStep'
data ReduceStepResult = Reduced ReduceWarning ReduceEffect State Contract
                      | NotReduced
                      | AmbiguousSlotIntervalReductionError
  deriving stock (Haskell.Show)


-- | Result of 'reduceContractUntilQuiescent'
data ReduceResult = ContractQuiescent Bool [ReduceWarning] [Payment] State Contract
                  | RRAmbiguousSlotIntervalError
  deriving stock (Haskell.Show)


-- | Warning of 'applyCases'
data ApplyWarning = ApplyNoWarning
                  | ApplyNonPositiveDeposit Party AccountId Token Integer
  deriving stock (Haskell.Show)


-- | Result of 'applyCases'
data ApplyResult = Applied ApplyWarning State Contract
                 | ApplyNoMatchError
  deriving stock (Haskell.Show)


-- | Result of 'applyAllInputs'
data ApplyAllResult = ApplyAllSuccess Bool [TransactionWarning] [Payment] State Contract
                    | ApplyAllNoMatchError
                    | ApplyAllAmbiguousSlotIntervalError
  deriving stock (Haskell.Show)


-- | Warnings during transaction computation
data TransactionWarning = TransactionNonPositiveDeposit Party AccountId Token Integer
                        | TransactionNonPositivePay AccountId Payee Token Integer
                        | TransactionPartialPay AccountId Payee Token Integer Integer
--                                                 ^ src    ^ dest     ^ paid   ^ expected
                        | TransactionShadowing ValueId Integer Integer
--                                                 oldVal ^  newVal ^
                        | TransactionAssertionFailed
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (Pretty)


-- | Transaction error
data TransactionError = TEAmbiguousSlotIntervalError
                      | TEApplyNoMatchError
                      | TEIntervalError IntervalError
                      | TEUselessTransaction
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)


{-| Marlowe transaction input.
-}
data TransactionInput = TransactionInput
    { txInterval :: SlotInterval
    , txInputs   :: [Input] }
  deriving stock (Haskell.Show, Haskell.Eq)

instance Pretty TransactionInput where
    prettyFragment tInp = text "TransactionInput" <> space <> lbrace <> line <> txIntLine <> line <> txInpLine
        where
            txIntLine = hang 2 $ text "txInterval = " <> prettyFragment (txInterval tInp) <> comma
            txInpLine = hang 2 $ text "txInputs = " <> prettyFragment (txInputs tInp) <> rbrace


{-| Marlowe transaction output.
-}
data TransactionOutput =
    TransactionOutput
        { txOutWarnings :: [TransactionWarning]
        , txOutPayments :: [Payment]
        , txOutState    :: State
        , txOutContract :: Contract }
    | Error TransactionError
  deriving stock (Haskell.Show)


{-|
    This data type is a content of a contract's /Datum/
-}
data MarloweData = MarloweData {
        marloweState    :: State,
        marloweContract :: Contract
    } deriving stock (Haskell.Show, Haskell.Eq, Generic)
      deriving anyclass (ToJSON, FromJSON)


data MarloweParams = MarloweParams {
        rolePayoutValidatorHash :: ValidatorHash,
        rolesCurrency           :: CurrencySymbol
    }
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (FromJSON,ToJSON)


-- | Empty State for a given minimal 'Slot'
emptyState :: Slot -> State
emptyState sn = State
    { accounts = Map.empty
    , choices  = Map.empty
    , boundValues = Map.empty
    , minSlot = sn }


-- | Check if a 'num' is withint a list of inclusive bounds.
inBounds :: ChosenNum -> [Bound] -> Bool
inBounds num = any (\(Bound l u) -> num >= l && num <= u)


{- Checks 'interval' and trim it if necessary. -}
fixInterval :: SlotInterval -> State -> IntervalResult
fixInterval interval state =
    case interval of
        (low, high)
          | high < low -> IntervalError (InvalidInterval interval)
          | otherwise -> let
            curMinSlot = minSlot state
            -- newLow is both new "low" and new "minSlot" (the lower bound for slotNum)
            newLow = max low curMinSlot
            -- We know high is greater or equal than newLow (prove)
            curInterval = (newLow, high)
            env = Environment { slotInterval = curInterval }
            newState = state { minSlot = newLow }
            in if high < curMinSlot then IntervalError (IntervalInPastError curMinSlot interval)
            else IntervalTrimmed env newState


{-|
  Evaluates @Value@ given current @State@ and @Environment@
-}
evalValue :: Environment -> State -> Value Observation -> Integer
evalValue env state value = let
    eval = evalValue env state
    in case value of
        AvailableMoney accId token -> moneyInAccount accId token (accounts state)
        Constant integer     -> integer
        NegValue val         -> negate (eval val)
        AddValue lhs rhs     -> eval lhs + eval rhs
        SubValue lhs rhs     -> eval lhs - eval rhs
        MulValue lhs rhs     -> eval lhs * eval rhs
        Scale s rhs          -> let (n, d) = (numerator s, denominator s)
                                    nn = eval rhs * n
                                    (q, r) = nn `quotRem` d in
                                if abs r * 2 < abs d then q else q + signum nn * signum d
        ChoiceValue choiceId ->
            case Map.lookup choiceId (choices state) of
                Just x  -> x
                Nothing -> 0
        SlotIntervalStart    -> getSlot (fst (slotInterval env))
        SlotIntervalEnd      -> getSlot (snd (slotInterval env))
        UseValue valId       ->
            case Map.lookup valId (boundValues state) of
                Just x  -> x
                Nothing -> 0
        Cond cond thn els    -> if evalObservation env state cond then eval thn else eval els
  where
    abs :: Integer -> Integer
    abs a = if a >= 0 then a else negate a

    signum :: Integer -> Integer
    signum x
      | x > 0 = 1
      | x == 0 = 0
      | otherwise = -1


-- | Evaluate 'Observation' to 'Bool'.
evalObservation :: Environment -> State -> Observation -> Bool
evalObservation env state obs = let
    evalObs = evalObservation env state
    evalVal = evalValue env state
    in case obs of
        AndObs lhs rhs          -> evalObs lhs && evalObs rhs
        OrObs lhs rhs           -> evalObs lhs || evalObs rhs
        NotObs subObs           -> not (evalObs subObs)
        ChoseSomething choiceId -> choiceId `Map.member` choices state
        ValueGE lhs rhs         -> evalVal lhs >= evalVal rhs
        ValueGT lhs rhs         -> evalVal lhs > evalVal rhs
        ValueLT lhs rhs         -> evalVal lhs < evalVal rhs
        ValueLE lhs rhs         -> evalVal lhs <= evalVal rhs
        ValueEQ lhs rhs         -> evalVal lhs == evalVal rhs
        TrueObs                 -> True
        FalseObs                -> False


-- | Pick the first account with money in it
refundOne :: Accounts -> Maybe ((Party, Money), Accounts)
refundOne accounts = case Map.toList accounts of
    [] -> Nothing
    ((accId, Token cur tok), balance) : rest ->
        if balance > 0
        then Just ((accId, Val.singleton cur tok balance), Map.fromList rest)
        else refundOne (Map.fromList rest)


-- | Obtains the amount of money available an account
moneyInAccount :: AccountId -> Token -> Accounts -> Integer
moneyInAccount accId token accounts = case Map.lookup (accId, token) accounts of
    Just x  -> x
    Nothing -> 0


-- | Sets the amount of money available in an account
updateMoneyInAccount :: AccountId -> Token -> Integer -> Accounts -> Accounts
updateMoneyInAccount accId token amount =
    if amount <= 0 then Map.delete (accId, token) else Map.insert (accId, token) amount


-- Add the given amount of money to an accoun (only if it is positive)
-- Return the updated Map
addMoneyToAccount :: AccountId -> Token -> Integer -> Accounts -> Accounts
addMoneyToAccount accId token amount accounts = let
    balance = moneyInAccount accId token accounts
    newBalance = balance + amount
    in if amount <= 0 then accounts
    else updateMoneyInAccount accId token newBalance accounts


{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
giveMoney :: AccountId -> Payee -> Token -> Integer -> Accounts -> (ReduceEffect, Accounts)
giveMoney accountId payee (Token cur tok) amount accounts = let
    newAccounts = case payee of
        Party _       -> accounts
        Account accId -> addMoneyToAccount accId (Token cur tok) amount accounts
    in (ReduceWithPayment (Payment accountId payee (Val.singleton cur tok amount)), newAccounts)


-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of

    Close -> case refundOne (accounts state) of
        Just ((party, money), newAccounts) -> let
            newState = state { accounts = newAccounts }
            in Reduced ReduceNoWarning (ReduceWithPayment (Payment party (Party party) money)) newState Close
        Nothing -> NotReduced

    Pay accId payee tok val cont -> let
        amountToPay = evalValue env state val
        in  if amountToPay <= 0
            then let
                warning = ReduceNonPositivePay accId payee tok amountToPay
                in Reduced warning ReduceNoPayment state cont
            else let
                balance    = moneyInAccount accId tok (accounts state)
                paidAmount = min balance amountToPay
                newBalance = balance - paidAmount
                newAccs = updateMoneyInAccount accId tok newBalance (accounts state)
                warning = if paidAmount < amountToPay
                          then ReducePartialPay accId payee tok paidAmount amountToPay
                          else ReduceNoWarning
                (payment, finalAccs) = giveMoney accId payee tok paidAmount newAccs
                newState = state { accounts = finalAccs }
                in Reduced warning payment newState cont

    If obs cont1 cont2 -> let
        cont = if evalObservation env state obs then cont1 else cont2
        in Reduced ReduceNoWarning ReduceNoPayment state cont

    When _ timeout cont -> let
        startSlot = fst (slotInterval env)
        endSlot   = snd (slotInterval env)
        -- if timeout in future – do not reduce
        in if endSlot < timeout then NotReduced
        -- if timeout in the past – reduce to timeout continuation
        else if timeout <= startSlot then Reduced ReduceNoWarning ReduceNoPayment state cont
        -- if timeout in the slot range – issue an ambiguity error
        else AmbiguousSlotIntervalReductionError

    Let valId val cont -> let
        evaluatedValue = evalValue env state val
        boundVals = boundValues state
        newState = state { boundValues = Map.insert valId evaluatedValue boundVals }
        warn = case Map.lookup valId boundVals of
              Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
              Nothing     -> ReduceNoWarning
        in Reduced warn ReduceNoPayment newState cont

    Assert obs cont -> let
        warning = if evalObservation env state obs
                  then ReduceNoWarning
                  else ReduceAssertionFailed
        in Reduced warning ReduceNoPayment state cont

-- | Reduce a contract until it cannot be reduced more
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent env state contract = let
    reductionLoop
      :: Bool -> Environment -> State -> Contract -> [ReduceWarning] -> [Payment] -> ReduceResult
    reductionLoop reduced env state contract warnings payments =
        case reduceContractStep env state contract of
            Reduced warning effect newState cont -> let
                newWarnings = if warning == ReduceNoWarning then warnings
                              else warning : warnings
                newPayments  = case effect of
                    ReduceWithPayment payment -> payment : payments
                    ReduceNoPayment           -> payments
                in reductionLoop True env newState cont newWarnings newPayments
            AmbiguousSlotIntervalReductionError -> RRAmbiguousSlotIntervalError
            -- this is the last invocation of reductionLoop, so we can reverse lists
            NotReduced -> ContractQuiescent reduced (reverse warnings) (reverse payments) state contract

    in reductionLoop False env state contract [] []


-- | Apply a single Input to the contract (assumes the contract is reduced)
applyCases :: Environment -> State -> Input -> [Case Contract] -> ApplyResult
applyCases env state input cases = case (input, cases) of
    (IDeposit accId1 party1 tok1 amount,
        Case (Deposit accId2 party2 tok2 val) cont : rest) ->
        if accId1 == accId2 && party1 == party2 && tok1 == tok2
                && amount == evalValue env state val
        then let
            warning = if amount > 0 then ApplyNoWarning
                      else ApplyNonPositiveDeposit party2 accId2 tok2 amount
            newAccounts = addMoneyToAccount accId1 tok1 amount (accounts state)
            newState = state { accounts = newAccounts }
            in Applied warning newState cont
        else applyCases env state input rest
    (IChoice choId1 choice, Case (Choice choId2 bounds) cont : rest) ->
        if choId1 == choId2 && inBounds choice bounds
        then let
            newState = state { choices = Map.insert choId1 choice (choices state) }
            in Applied ApplyNoWarning newState cont
        else applyCases env state input rest
    (INotify, Case (Notify obs) cont : _)
        | evalObservation env state obs -> Applied ApplyNoWarning state cont
    (_, _ : rest) -> applyCases env state input rest
    (_, []) -> ApplyNoMatchError


-- | Apply a single @Input@ to a current contract
applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput env state input (When cases _ _) = applyCases env state input cases
applyInput _ _ _ _                          = ApplyNoMatchError


-- | Propagate 'ReduceWarning' to 'TransactionWarning'
convertReduceWarnings :: [ReduceWarning] -> [TransactionWarning]
convertReduceWarnings = foldr (\warn acc -> case warn of
    ReduceNoWarning -> acc
    ReduceNonPositivePay accId payee tok amount ->
        TransactionNonPositivePay accId payee tok amount : acc
    ReducePartialPay accId payee tok paid expected ->
        TransactionPartialPay accId payee tok paid expected : acc
    ReduceShadowing valId oldVal newVal ->
        TransactionShadowing valId oldVal newVal : acc
    ReduceAssertionFailed ->
        TransactionAssertionFailed : acc
    ) []


-- | Apply a list of Inputs to the contract
applyAllInputs :: Environment -> State -> Contract -> [Input] -> ApplyAllResult
applyAllInputs env state contract inputs = let
    applyAllLoop
        :: Bool
        -> Environment
        -> State
        -> Contract
        -> [Input]
        -> [TransactionWarning]
        -> [Payment]
        -> ApplyAllResult
    applyAllLoop contractChanged env state contract inputs warnings payments =
        case reduceContractUntilQuiescent env state contract of
            RRAmbiguousSlotIntervalError -> ApplyAllAmbiguousSlotIntervalError
            ContractQuiescent reduced reduceWarns pays curState cont -> case inputs of
                [] -> ApplyAllSuccess
                    (contractChanged || reduced)
                    (warnings ++ convertReduceWarnings reduceWarns)
                    (payments ++ pays)
                    curState
                    cont
                (input : rest) -> case applyInput env curState input cont of
                    Applied applyWarn newState cont ->
                        applyAllLoop
                            True
                            env
                            newState
                            cont
                            rest
                            (warnings
                                ++ convertReduceWarnings reduceWarns
                                ++ convertApplyWarning applyWarn)
                            (payments ++ pays)
                    ApplyNoMatchError -> ApplyAllNoMatchError
    in applyAllLoop False env state contract inputs [] []
  where
    convertApplyWarning :: ApplyWarning -> [TransactionWarning]
    convertApplyWarning warn =
        case warn of
            ApplyNoWarning -> []
            ApplyNonPositiveDeposit party accId tok amount ->
                [TransactionNonPositiveDeposit party accId tok amount]

isClose :: Contract -> Bool
isClose Close = True
isClose _     = False

-- | Try to compute outputs of a transaction given its inputs, a contract, and it's @State@
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract = let
    inputs = txInputs tx
    in case fixInterval (txInterval tx) state of
        IntervalTrimmed env fixState -> case applyAllInputs env fixState contract inputs of
            ApplyAllSuccess reduced warnings payments newState cont ->
                    if not reduced && (not (isClose contract) || (Map.null $ accounts state))
                    then Error TEUselessTransaction
                    else TransactionOutput { txOutWarnings = warnings
                                           , txOutPayments = payments
                                           , txOutState = newState
                                           , txOutContract = cont }
            ApplyAllNoMatchError -> Error TEApplyNoMatchError
            ApplyAllAmbiguousSlotIntervalError -> Error TEAmbiguousSlotIntervalError
        IntervalError error -> Error (TEIntervalError error)


-- | Calculates an upper bound for the maximum lifespan of a contract
contractLifespanUpperBound :: Contract -> Slot
contractLifespanUpperBound contract = case contract of
    Close -> 0
    Pay _ _ _ _ cont -> contractLifespanUpperBound cont
    If _ contract1 contract2 ->
        max (contractLifespanUpperBound contract1) (contractLifespanUpperBound contract2)
    When cases timeout subContract -> let
        contractsLifespans = fmap (\(Case _ cont) -> contractLifespanUpperBound cont) cases
        in Haskell.maximum (timeout : contractLifespanUpperBound subContract : contractsLifespans)
    Let _ _ cont -> contractLifespanUpperBound cont
    Assert _ cont -> contractLifespanUpperBound cont


totalBalance :: Accounts -> Money
totalBalance accounts = foldMap
    (\((_, Token cur tok), balance) -> Val.singleton cur tok balance)
    (Map.toList accounts)


{-|
    Check that all accounts have positive balance.
 -}
validateBalances :: State -> Bool
validateBalances State{..} = all (\(_, balance) -> balance > 0) (Map.toList accounts)


-- Typeclass instances

customOptions :: Options
customOptions = defaultOptions
                { unwrapUnaryRecords = True
                , sumEncoding = TaggedObject { tagFieldName = "tag", contentsFieldName = "contents" }
                }

getInteger :: Scientific -> Parser Integer
getInteger x = case (floatingOrInteger x :: Either Haskell.Double Integer) of
                 Right a -> return a
                 Left _  -> Haskell.fail "Account number is not an integer"

withInteger :: JSON.Value -> Parser Integer
withInteger = withScientific "" getInteger

instance FromJSON State where
  parseJSON = withObject "State" (\v ->
         State <$> (v .: "accounts")
               <*> (v .: "choices")
               <*> (v .: "boundValues")
               <*> (Slot <$> (withInteger =<< (v .: "minSlot")))
                                 )

instance ToJSON State where
  toJSON State { accounts = a
               , choices = c
               , boundValues = bv
               , minSlot = Slot ms } = object
        [ "accounts" .= a
        , "choices" .= c
        , "boundValues" .= bv
        , "minSlot" .= ms ]

instance FromJSON Party where
  parseJSON = withObject "Party" (\v ->
        (PK . PubKeyHash <$> (JSON.decodeByteString =<< (v .: "pk_hash")))
    <|> (Role . Val.TokenName . encodeUtf8 <$> (v .: "role_token"))
                                 )
instance ToJSON Party where
    toJSON (PK pkh) = object
        [ "pk_hash" .= (JSON.String $ JSON.encodeByteString $ getPubKeyHash pkh) ]
    toJSON (Role (Val.TokenName name)) = object
        [ "role_token" .= (JSON.String $ decodeUtf8 name) ]


instance FromJSON ChoiceId where
  parseJSON = withObject "ChoiceId" (\v ->
       ChoiceId <$> (encodeUtf8 <$> (v .: "choice_name"))
                <*> (v .: "choice_owner")
                                    )

instance ToJSON ChoiceId where
  toJSON (ChoiceId name party) = object [ "choice_name" .= (JSON.String $ decodeUtf8 name)
                                        , "choice_owner" .= party
                                        ]


instance FromJSON Token where
  parseJSON = withObject "Token" (\v ->
       Token <$> (CurrencySymbol <$> (JSON.decodeByteString =<< (v .: "currency_symbol")))
             <*> (Val.TokenName . encodeUtf8 <$> (v .: "token_name"))
                                 )

instance ToJSON Token where
  toJSON (Token currSym tokName) = object
      [ "currency_symbol" .= (JSON.String $ JSON.encodeByteString $ unCurrencySymbol currSym)
      , "token_name" .= (JSON.String $ decodeUtf8 $ unTokenName tokName)
      ]

instance FromJSON ValueId where
    parseJSON = withText "ValueId" $ return . ValueId . encodeUtf8
instance ToJSON ValueId where
    toJSON (ValueId x) = JSON.String (decodeUtf8 x)


instance FromJSON (Value Observation) where
  parseJSON (Object v) =
        (AvailableMoney <$> (v .: "in_account")
                        <*> (v .: "amount_of_token"))
    <|> (NegValue <$> (v .: "negate"))
    <|> (AddValue <$> (v .: "add")
                  <*> (v .: "and"))
    <|> (SubValue <$> (v .: "value")
                  <*> (v .: "minus"))
    <|> (do maybeDiv <- v .:? "divide_by"
            case maybeDiv :: Maybe Scientific of
              Nothing -> MulValue <$> (v .: "multiply")
                                  <*> (v .: "times")
              Just divi -> Scale <$> ((%) <$> (getInteger =<< (v .: "times")) <*> getInteger divi)
                                 <*> (v .: "multiply"))
    <|> (ChoiceValue <$> (v .: "value_of_choice"))
    <|> (UseValue <$> (v .: "use_value"))
    <|> (Cond <$> (v .: "if")
              <*> (v .: "then")
              <*> (v .: "else"))
  parseJSON (String "slot_interval_start") = return SlotIntervalStart
  parseJSON (String "slot_interval_end") = return SlotIntervalEnd
  parseJSON (Number n) = Constant <$> getInteger n
  parseJSON _ = Haskell.fail "Value must be either an object or an integer"
instance ToJSON (Value Observation) where
  toJSON (AvailableMoney accountId token) = object
      [ "amount_of_token" .= token
      , "in_account" .= accountId
      ]
  toJSON (Constant x) = toJSON x
  toJSON (NegValue x) = object
      [ "negate" .= x ]
  toJSON (AddValue lhs rhs) = object
      [ "add" .= lhs
      , "and" .= rhs
      ]
  toJSON (SubValue lhs rhs) = object
      [ "value" .= lhs
      , "minus" .= rhs
      ]
  toJSON (MulValue lhs rhs) = object
      [ "multiply" .= lhs
      , "times" .= rhs
      ]
  toJSON (Scale rat v) = object
      [ "multiply" .= v
      , "times" .= num
      , "divide_by" .= den
      ]
    where num = numerator rat
          den = denominator rat
  toJSON (ChoiceValue choiceId) = object
      [ "value_of_choice" .= choiceId ]
  toJSON SlotIntervalStart = JSON.String $ pack "slot_interval_start"
  toJSON SlotIntervalEnd = JSON.String $ pack "slot_interval_end"
  toJSON (UseValue valueId) = object
      [ "use_value" .= valueId ]
  toJSON (Cond obs tv ev) = object
      [ "if" .= obs
      , "then" .= tv
      , "else" .= ev
      ]


instance FromJSON Observation where
  parseJSON (Bool True) = return TrueObs
  parseJSON (Bool False) = return FalseObs
  parseJSON (Object v) =
        (AndObs <$> (v .: "both")
                <*> (v .: "and"))
    <|> (OrObs <$> (v .: "either")
               <*> (v .: "or"))
    <|> (NotObs <$> (v .: "not"))
    <|> (ChoseSomething <$> (v .: "chose_something_for"))
    <|> (ValueGE <$> (v .: "value")
                 <*> (v .: "ge_than"))
    <|> (ValueGT <$> (v .: "value")
                 <*> (v .: "gt"))
    <|> (ValueLT <$> (v .: "value")
                 <*> (v .: "lt"))
    <|> (ValueLE <$> (v .: "value")
                 <*> (v .: "le_than"))
    <|> (ValueEQ <$> (v .: "value")
                 <*> (v .: "equal_to"))
  parseJSON _ = Haskell.fail "Observation must be either an object or a boolean"

instance ToJSON Observation where
  toJSON (AndObs lhs rhs) = object
      [ "both" .= lhs
      , "and" .= rhs
      ]
  toJSON (OrObs lhs rhs) = object
      [ "either" .= lhs
      , "or" .= rhs
      ]
  toJSON (NotObs v) = object
      [ "not" .= v ]
  toJSON (ChoseSomething choiceId) = object
      [ "chose_something_for" .= choiceId ]
  toJSON (ValueGE lhs rhs) = object
      [ "value" .= lhs
      , "ge_than" .= rhs
      ]
  toJSON (ValueGT lhs rhs) = object
      [ "value" .= lhs
      , "gt" .= rhs
      ]
  toJSON (ValueLT lhs rhs) = object
      [ "value" .= lhs
      , "lt" .= rhs
      ]
  toJSON (ValueLE lhs rhs) = object
      [ "value" .= lhs
      , "le_than" .= rhs
      ]
  toJSON (ValueEQ lhs rhs) = object
      [ "value" .= lhs
      , "equal_to" .= rhs
      ]
  toJSON TrueObs = toJSON True
  toJSON FalseObs = toJSON False


instance FromJSON Bound where
  parseJSON = withObject "Bound" (\v ->
       Bound <$> (getInteger =<< (v .: "from"))
             <*> (getInteger =<< (v .: "to"))
                                 )
instance ToJSON Bound where
  toJSON (Bound from to) = object
      [ "from" .= from
      , "to" .= to
      ]

instance FromJSON Action where
  parseJSON = withObject "Action" (\v ->
       (Deposit <$> (v .: "into_account")
                <*> (v .: "party")
                <*> (v .: "of_token")
                <*> (v .: "deposits"))
   <|> (Choice <$> (v .: "for_choice")
               <*> ((v .: "choose_between") >>=
                    withArray "Bound list" (\bl ->
                      mapM parseJSON (F.toList bl)
                                            )))
   <|> (Notify <$> (v .: "notify_if"))
                                  )
instance ToJSON Action where
  toJSON (Deposit accountId party token val) = object
      [ "into_account" .= accountId
      , "party" .= party
      , "of_token" .= token
      , "deposits" .= val
      ]
  toJSON (Choice choiceId bounds) = object
      [ "for_choice" .= choiceId
      , "choose_between" .= toJSONList (map toJSON bounds)
      ]
  toJSON (Notify obs) = object
      [ "notify_if" .= obs ]


instance FromJSON Payee where
  parseJSON = withObject "Payee" (\v ->
                (Account <$> (v .: "account"))
            <|> (Party <$> (v .: "party")))

instance ToJSON Payee where
  toJSON (Account acc) = object ["account" .= acc]
  toJSON (Party party) = object ["party" .= party]


instance FromJSON a => FromJSON (Case a) where
  parseJSON = withObject "Case" (\v ->
       Case <$> (v .: "case")
            <*> (v .: "then")
                                )
instance ToJSON a => ToJSON (Case a) where
  toJSON (Case act cont) = object
      [ "case" .= act
      , "then" .= cont
      ]


instance FromJSON Contract where
  parseJSON (String "close") = return Close
  parseJSON (Object v) =
        (Pay <$> (v .: "from_account")
             <*> (v .: "to")
             <*> (v .: "token")
             <*> (v .: "pay")
             <*> (v .: "then"))
    <|> (If <$> (v .: "if")
            <*> (v .: "then")
            <*> (v .: "else"))
    <|> (When <$> ((v .: "when") >>=
                   withArray "Case list" (\cl ->
                     mapM parseJSON (F.toList cl)
                                          ))
              <*> (Slot <$> (withInteger =<< (v .: "timeout")))
              <*> (v .: "timeout_continuation"))
    <|> (Let <$> (v .: "let")
             <*> (v .: "be")
             <*> (v .: "then"))
    <|> (Assert <$> (v .: "assert")
                <*> (v .: "then"))
  parseJSON _ = Haskell.fail "Contract must be either an object or a the string \"close\""

instance ToJSON Contract where
  toJSON Close = JSON.String $ pack "close"
  toJSON (Pay accountId payee token value contract) = object
      [ "from_account" .= accountId
      , "to" .= payee
      , "token" .= token
      , "pay" .= value
      , "then" .= contract
      ]
  toJSON (If obs cont1 cont2) = object
      [ "if" .= obs
      , "then" .= cont1
      , "else" .= cont2
      ]
  toJSON (When caseList timeout cont) = object
      [ "when" .= toJSONList (map toJSON caseList)
      , "timeout" .= getSlot timeout
      , "timeout_continuation" .= cont
      ]
  toJSON (Let valId value cont) = object
      [ "let" .= valId
      , "be" .= value
      , "then" .= cont
      ]
  toJSON (Assert obs cont) = object
      [ "assert" .= obs
      , "then" .= cont
      ]

instance FromJSON TransactionInput where
  parseJSON (Object v) =
        TransactionInput <$> (parseSlotInterval =<< (v .: "tx_interval"))
                         <*> ((v .: "tx_inputs") >>=
                   withArray "Transaction input list" (\cl ->
                     mapM parseJSON (F.toList cl)
                                                      ))
    where parseSlotInterval = withObject "SlotInterval" (\v ->
            do from <- Slot <$> (withInteger =<< (v .: "from"))
               to <- Slot <$> (withInteger =<< (v .: "to"))
               return (from, to)
                                                      )
  parseJSON _ = Haskell.fail "TransactionInput must be an object"

instance ToJSON TransactionInput where
  toJSON (TransactionInput (Slot from, Slot to) txInps) = object
      [ "tx_interval" .= slotIntervalJSON
      , "tx_inputs" .= toJSONList (map toJSON txInps)
      ]
    where slotIntervalJSON = object [ "from" .= from
                                    , "to" .= to
                                    ]

instance FromJSON TransactionWarning where
  parseJSON (String "assertion_failed") = return TransactionAssertionFailed
  parseJSON (Object v) =
        (TransactionNonPositiveDeposit <$> (v .: "party")
                                       <*> (v .: "in_account")
                                       <*> (v .: "of_token")
                                       <*> (v .: "asked_to_deposit"))
    <|> (do maybeButOnlyPaid <- v .:? "but_only_paid"
            case maybeButOnlyPaid :: Maybe Scientific of
              Nothing -> TransactionNonPositivePay <$> (v .: "account")
                                                   <*> (v .: "to_payee")
                                                   <*> (v .: "of_token")
                                                   <*> (v .: "asked_to_pay")
              Just butOnlyPaid -> TransactionPartialPay <$> (v .: "account")
                                                        <*> (v .: "to_payee")
                                                        <*> (v .: "of_token")
                                                        <*> getInteger butOnlyPaid
                                                        <*> (v .: "asked_to_pay"))
    <|> (TransactionShadowing <$> (v .: "value_id")
                              <*> (v .: "had_value")
                              <*> (v .: "is_now_assigned"))
  parseJSON _ = Haskell.fail "Contract must be either an object or a the string \"close\""

instance ToJSON TransactionWarning where
  toJSON (TransactionNonPositiveDeposit party accId tok amount) = object
      [ "party" .= party
      , "asked_to_deposit" .= amount
      , "of_token" .= tok
      , "in_account" .= accId
      ]
  toJSON (TransactionNonPositivePay accId payee tok amount) = object
      [ "account" .= accId
      , "asked_to_pay" .= amount
      , "of_token" .= tok
      , "to_payee" .= payee
      ]
  toJSON (TransactionPartialPay accId payee tok paid expected) = object
      [ "account" .= accId
      , "asked_to_pay" .= expected
      , "of_token" .= tok
      , "to_payee" .= payee
      , "but_only_paid" .= paid
      ]
  toJSON (TransactionShadowing valId oldVal newVal) = object
      [ "value_id" .= valId
      , "had_value" .= oldVal
      , "is_now_assigned" .= newVal
      ]
  toJSON TransactionAssertionFailed = JSON.String $ pack "assertion_failed"


instance Eq Party where
    {-# INLINABLE (==) #-}
    (PK p1) == (PK p2)     = p1 == p2
    (Role r1) == (Role r2) = r1 == r2
    _ == _                 = False


instance Eq ChoiceId where
    {-# INLINABLE (==) #-}
    (ChoiceId n1 p1) == (ChoiceId n2 p2) = n1 == n2 && p1 == p2

instance Eq Token where
    {-# INLINABLE (==) #-}
    (Token n1 p1) == (Token n2 p2) = n1 == n2 && p1 == p2

instance Eq ValueId where
    {-# INLINABLE (==) #-}
    (ValueId n1) == (ValueId n2) = n1 == n2


instance Pretty ValueId where
    prettyFragment (ValueId n) = prettyFragment n

instance Eq Payee where
    {-# INLINABLE (==) #-}
    Account acc1 == Account acc2 = acc1 == acc2
    Party p1 == Party p2         = p1 == p2
    _ == _                       = False


instance Eq Payment where
    {-# INLINABLE (==) #-}
    Payment a1 p1 m1 == Payment a2 p2 m2 = a1 == a2 && p1 == p2 && m1 == m2


instance Eq ReduceWarning where
    {-# INLINABLE (==) #-}
    ReduceNoWarning == ReduceNoWarning = True
    (ReduceNonPositivePay acc1 p1 tn1 a1) == (ReduceNonPositivePay acc2 p2 tn2 a2) =
        acc1 == acc2 && p1 == p2 && tn1 == tn2 && a1 == a2
    (ReducePartialPay acc1 p1 tn1 a1 e1) == (ReducePartialPay acc2 p2 tn2 a2 e2) =
        acc1 == acc2 && p1 == p2 && tn1 == tn2 && a1 == a2 && e1 == e2
    (ReduceShadowing v1 old1 new1) == (ReduceShadowing v2 old2 new2) =
        v1 == v2 && old1 == old2 && new1 == new2
    _ == _ = False


instance Eq ReduceEffect where
    {-# INLINABLE (==) #-}
    ReduceNoPayment == ReduceNoPayment           = True
    ReduceWithPayment p1 == ReduceWithPayment p2 = p1 == p2
    _ == _                                       = False


instance Eq a => Eq (Value a) where
    {-# INLINABLE (==) #-}
    AvailableMoney acc1 tok1 == AvailableMoney acc2 tok2 =
        acc1 == acc2 && tok1 == tok2
    Constant i1 == Constant i2 = i1 == i2
    NegValue val1 == NegValue val2 = val1 == val2
    AddValue val1 val2 == AddValue val3 val4 = val1 == val3 && val2 == val4
    SubValue val1 val2 == SubValue val3 val4 = val1 == val3 && val2 == val4
    MulValue val1 val2 == MulValue val3 val4 = val1 == val3 && val2 == val4
    Scale s1 val1 == Scale s2 val2 = s1 == s2 && val1 == val2
    ChoiceValue cid1 == ChoiceValue cid2 = cid1 == cid2
    SlotIntervalStart == SlotIntervalStart = True
    SlotIntervalEnd   == SlotIntervalEnd   = True
    UseValue val1 == UseValue val2 = val1 == val2
    Cond obs1 thn1 els1 == Cond obs2 thn2 els2 =  obs1 == obs2 && thn1 == thn2 && els1 == els2
    _ == _ = False

instance Eq Observation where
    {-# INLINABLE (==) #-}
    AndObs o1l o2l == AndObs o1r o2r           = o1l == o1r && o2l == o2r
    OrObs  o1l o2l == OrObs  o1r o2r           = o1l == o1r && o2l == o2r
    NotObs ol == NotObs or                     = ol == or
    ChoseSomething cid1 == ChoseSomething cid2 = cid1 == cid2
    ValueGE v1l v2l == ValueGE v1r v2r         = v1l == v1r && v2l == v2r
    ValueGT v1l v2l == ValueGT v1r v2r         = v1l == v1r && v2l == v2r
    ValueLT v1l v2l == ValueLT v1r v2r         = v1l == v1r && v2l == v2r
    ValueLE v1l v2l == ValueLE v1r v2r         = v1l == v1r && v2l == v2r
    ValueEQ v1l v2l == ValueEQ v1r v2r         = v1l == v1r && v2l == v2r
    TrueObs  == TrueObs                        = True
    FalseObs == FalseObs                       = True
    _ == _                                     = False


instance Eq Action where
    {-# INLINABLE (==) #-}
    Deposit acc1 party1 tok1 val1 == Deposit acc2 party2 tok2 val2 =
        acc1 == acc2 && party1 == party2 && tok1 == tok2 && val1 == val2
    Choice cid1 bounds1 == Choice cid2 bounds2 =
        cid1 == cid2 && length bounds1 == length bounds2 && let
            bounds = zip bounds1 bounds2
            checkBound (Bound low1 high1, Bound low2 high2) = low1 == low2 && high1 == high2
            in all checkBound bounds
    Notify obs1 == Notify obs2 = obs1 == obs2
    _ == _ = False


instance Eq Contract where
    {-# INLINABLE (==) #-}
    Close == Close = True
    Pay acc1 payee1 tok1 value1 cont1 == Pay acc2 payee2 tok2 value2 cont2 =
        acc1 == acc2 && payee1 == payee2 && tok1 == tok2 && value1 == value2 && cont1 == cont2
    If obs1 cont1 cont2 == If obs2 cont3 cont4 =
        obs1 == obs2 && cont1 == cont3 && cont2 == cont4
    When cases1 timeout1 cont1 == When cases2 timeout2 cont2 =
        timeout1 == timeout2 && cont1 == cont2
        && length cases1 == length cases2
        && let cases = zip cases1 cases2
               checkCase (Case action1 cont1, Case action2 cont2) =
                    action1 == action2 && cont1 == cont2
           in all checkCase cases
    Let valId1 val1 cont1 == Let valId2 val2 cont2 =
        valId1 == valId2 && val1 == val2 && cont1 == cont2
    Assert obs1 cont1 == Assert obs2 cont2 = obs1 == obs2 && cont1 == cont2
    _ == _ = False


instance Eq State where
    {-# INLINABLE (==) #-}
    l == r = minSlot l == minSlot r
        && accounts l == accounts r
        && choices l == choices r
        && boundValues l == boundValues r


-- Lifting data types to Plutus Core
makeLift ''Party
makeIsDataIndexed ''Party [('PK,0),('Role,1)]
makeLift ''ChoiceId
makeIsDataIndexed ''ChoiceId [('ChoiceId,0)]
makeLift ''Token
makeIsDataIndexed ''Token [('Token,0)]
makeLift ''ValueId
makeIsDataIndexed ''ValueId [('ValueId,0)]
makeLift ''Value
makeIsDataIndexed ''Value [
    ('AvailableMoney,0),
    ('Constant,1),
    ('NegValue,2),
    ('AddValue,3),
    ('SubValue,4),
    ('MulValue,5),
    ('Scale,6),
    ('ChoiceValue,7),
    ('SlotIntervalStart, 8),
    ('SlotIntervalEnd,9),
    ('UseValue,10),
    ('Cond,11)
    ]
makeLift ''Observation
makeIsDataIndexed ''Observation [
    ('AndObs,0),
    ('OrObs,1),
    ('NotObs,2),
    ('ChoseSomething,3),
    ('ValueGE,4),
    ('ValueGT,5),
    ('ValueLT,6),
    ('ValueLE,7),
    ('ValueEQ,8),
    ('TrueObs,9),
    ('FalseObs,10)
    ]
makeLift ''Bound
makeIsDataIndexed ''Bound [('Bound,0)]
makeLift ''Action
makeIsDataIndexed ''Action [('Deposit,0),('Choice,1),('Notify,2)]
makeLift ''Case
makeIsDataIndexed ''Case [('Case,0)]
makeLift ''Payee
makeIsDataIndexed ''Payee [('Account,0),('Party,1)]
makeLift ''Contract
makeIsDataIndexed ''Contract [
    ('Close,0),
    ('Pay,1),
    ('If,2),
    ('When,3),
    ('Let,4),
    ('Assert,5)
    ]
makeLift ''State
makeIsDataIndexed ''State [('State,0)]
makeLift ''Environment
makeLift ''Input
makeIsDataIndexed ''Input [('IDeposit,0),('IChoice,1),('INotify,2)]
makeLift ''IntervalError
makeLift ''IntervalResult
makeLift ''Payment
makeLift ''ReduceEffect
makeLift ''ReduceWarning
makeLift ''ReduceStepResult
makeLift ''ReduceResult
makeLift ''ApplyWarning
makeLift ''ApplyResult
makeLift ''ApplyAllResult
makeLift ''TransactionWarning
makeLift ''TransactionError
makeLift ''TransactionOutput
makeLift ''MarloweData
makeIsDataIndexed ''MarloweData [('MarloweData,0)]
makeLift ''MarloweParams
