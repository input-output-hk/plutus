module Marlowe.Semantics where

import Prelude
import Decode.Helpers ((<|>))
import Control.Monad.Except.Trans (ExceptT)
import Data.Array (catMaybes)
import Data.BigInteger (BigInteger, fromInt)
import Data.Foldable (class Foldable, any, foldl, minimum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.Lens (Lens', over, to, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), fromFoldable, reverse, (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs, signum)
import Data.String (toLower)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign, ForeignError(..), fail)
import Foreign.Class (class Encode, class Decode, encode, decode)
import Foreign.Generic (genericDecode, genericEncode)
import Foreign.Generic.Class (Options, defaultOptions, aesonSumEncoding)
import Foreign.Index (hasProperty, readProp)
import Text.Pretty (class Args, class Pretty, genericHasArgs, genericHasNestedArgs, genericPretty, text)

decodeProp :: forall a. Decode a => String → Foreign → ExceptT (NonEmptyList ForeignError) Identity a
decodeProp key obj = decode =<< readProp key obj

type PubKey
  = String

data Party
  = PK PubKey
  | Role TokenName

derive instance genericParty :: Generic Party _

derive instance eqParty :: Eq Party

derive instance ordParty :: Ord Party

instance encodeJsonParty :: Encode Party where
  encode (PK pubKey) = encode { pk_hash: pubKey }
  encode (Role tokName) = encode { role_token: tokName }

instance decodeJsonParty :: Decode Party where
  decode a =
    (PK <$> decodeProp "pk_hash" a)
      <|> ( \_ ->
            Role <$> decodeProp "role_token" a
        )

instance showParty :: Show Party where
  show = genericShow

instance prettyParty :: Pretty Party where
  pretty = genericPretty

instance hasArgsParty :: Args Party where
  hasArgs = genericHasArgs
  hasNestedArgs = genericHasNestedArgs

type Timeout
  = Slot

type Money
  = Assets

type CurrencySymbol
  = String

type TokenName
  = String

data Token
  = Token CurrencySymbol TokenName

instance encodeJsonToken :: Encode Token where
  encode (Token cur tok) =
    encode
      { currency_symbol: cur
      , token_name: tok
      }

type TokenJson
  = { currency :: { unCurrencySymbol :: String }, token :: { unTokenName :: String } }

instance decodeJsonToken :: Decode Token where
  decode a =
    ( Token <$> decodeProp "currency_symbol" a
        <*> decodeProp "token_name" a
    )

derive instance genericToken :: Generic Token _

instance eqToken :: Eq Token where
  eq (Token cur1 tok1) (Token cur2 tok2) = eq (toLower cur1) (toLower cur2) && eq tok1 tok2

instance ordToken :: Ord Token where
  compare (Token cur1 tok1) (Token cur2 tok2) = case compare (toLower cur1) (toLower cur2) of
    EQ -> compare tok1 tok2
    other -> other

instance showToken :: Show Token where
  show (Token cur tok) = genericShow (Token (toLower cur) tok)

instance prettyToken :: Pretty Token where
  pretty = genericPretty

instance hasArgsToken :: Args Token where
  hasArgs = genericHasArgs
  hasNestedArgs = genericHasNestedArgs

type ChosenNum
  = BigInteger

type Accounts
  = Map (Tuple AccountId Token) BigInteger

type ChoiceName
  = String

newtype Assets
  = Assets (Map CurrencySymbol (Map TokenName BigInteger))

derive instance genericAssets :: Generic Assets _

derive instance newtypeAssets :: Newtype Assets _

derive instance eqAssets :: Eq Assets

derive instance ordAssets :: Ord Assets

derive newtype instance showAssets :: Show Assets

derive newtype instance encodeAssets :: Encode Assets

derive newtype instance decodeAssets :: Decode Assets

instance semigroupAssets :: Semigroup Assets where
  append (Assets a) (Assets b) = Assets (Map.unionWith f a b)
    where
    f = Map.unionWith (+)

instance monoidAssets :: Monoid Assets where
  mempty = Assets mempty

newtype Slot
  = Slot BigInteger

instance encodeJsonSlot :: Encode Slot where
  encode (Slot n) = encode n

instance decodeJsonSlot :: Decode Slot where
  decode a = Slot <$> decode a

derive instance genericSlot :: Generic Slot _

derive instance newtypeSlot :: Newtype Slot _

derive instance eqSlot :: Eq Slot

derive instance ordSlot :: Ord Slot

derive newtype instance showSlot :: Show Slot

derive newtype instance semiRingSlot :: Semiring Slot

derive newtype instance ringSlot :: Ring Slot

instance commutativeRingSlot :: CommutativeRing Slot

derive newtype instance euclideanRingSlot :: EuclideanRing Slot

derive newtype instance prettySlot :: Pretty Slot

derive newtype instance hasArgsSlot :: Args Slot

newtype Ada
  = Lovelace BigInteger

instance encodeJsonAda :: Encode Ada where
  encode a = genericEncode aesonCompatibleOptions a

instance decodeJsonAda :: Decode Ada where
  decode a = genericDecode aesonCompatibleOptions a

derive instance genericAda :: Generic Ada _

derive instance newtypeAda :: Newtype Ada _

derive instance eqAda :: Eq Ada

derive instance ordAda :: Ord Ada

derive newtype instance showAda :: Show Ada

derive newtype instance semiringAda :: Semiring Ada

derive newtype instance ringAda :: Ring Ada

derive newtype instance euclideanRingAda :: EuclideanRing Ada

instance commutativeRingAda :: CommutativeRing Ada

type AccountId
  = Party

data ChoiceId
  = ChoiceId String Party

derive instance genericChoiceId :: Generic ChoiceId _

derive instance eqChoiceId :: Eq ChoiceId

derive instance ordChoiceId :: Ord ChoiceId

instance encodeJsonChoiceId :: Encode ChoiceId where
  encode (ChoiceId name owner) =
    encode
      { choice_name: name
      , choice_owner: owner
      }

instance decodeJsonChoiceId :: Decode ChoiceId where
  decode a =
    ( ChoiceId <$> decodeProp "choice_name" a
        <*> decodeProp "choice_owner" a
    )

instance showChoiceId :: Show ChoiceId where
  show (ChoiceId name owner) = "(ChoiceId " <> show name <> " " <> show owner <> ")"

instance prettyChoiceId :: Pretty ChoiceId where
  pretty = genericPretty

instance hasArgsChoiceId :: Args ChoiceId where
  hasArgs = genericHasArgs
  hasNestedArgs = genericHasNestedArgs

choiceOwner :: ChoiceId -> Party
choiceOwner (ChoiceId _ owner) = owner

newtype ValueId
  = ValueId String

derive instance genericValueId :: Generic ValueId _

derive instance newtypeValueId :: Newtype ValueId _

derive instance eqValueId :: Eq ValueId

derive instance ordValueId :: Ord ValueId

instance encodeJsonValueId :: Encode ValueId where
  encode (ValueId a) = encode a

instance decodeJsonValueId :: Decode ValueId where
  decode a = ValueId <$> decode a

instance showValueId :: Show ValueId where
  show (ValueId valueId) = show valueId

instance prettyValueId :: Pretty ValueId where
  pretty (ValueId valueId) = text $ show valueId

instance hasArgsValueId :: Args ValueId where
  hasArgs _ = false
  hasNestedArgs _ = false

data Rational
  = Rational BigInteger BigInteger

derive instance genericRational :: Generic Rational _

instance eqRational :: Eq Rational where
  eq (Rational n1 d1) (Rational n2 d2) = eq (d1 * n2) (d2 * n1)

derive instance ordRational :: Ord Rational

instance encodeJsonRational :: Encode Rational where
  encode a = genericEncode aesonCompatibleOptions a

instance decodeJsonRational :: Decode Rational where
  decode a = genericDecode aesonCompatibleOptions a

instance showRational :: Show Rational where
  show (Rational n d) = "(" <> show n <> "%" <> show d <> ")"

instance prettyRational :: Pretty Rational where
  pretty r = text $ show r

instance hasArgsRational :: Args Rational where
  hasArgs a = false
  hasNestedArgs a = false

data Value
  = AvailableMoney AccountId Token
  | Constant BigInteger
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | Scale Rational Value
  | ChoiceValue ChoiceId
  | SlotIntervalStart
  | SlotIntervalEnd
  | UseValue ValueId
  | Cond Observation Value Value

derive instance genericValue :: Generic Value _

derive instance eqValue :: Eq Value

derive instance ordValue :: Ord Value

instance encodeJsonValue :: Encode Value where
  encode (AvailableMoney accId tok) =
    encode
      { amount_of_token: tok
      , in_account: accId
      }
  encode (Constant val) = encode val
  encode (NegValue val) =
    encode
      { negate: val
      }
  encode (AddValue lhs rhs) =
    encode
      { add: lhs
      , and: rhs
      }
  encode (SubValue lhs rhs) =
    encode
      { value: lhs
      , minus: rhs
      }
  encode (MulValue lhs rhs) =
    encode
      { multiply: lhs
      , times: rhs
      }
  encode (Scale (Rational num den) val) =
    encode
      { multiply: val
      , times: num
      , divide_by: den
      }
  encode (ChoiceValue choiceId) =
    encode
      { value_of_choice: choiceId
      }
  encode SlotIntervalStart = encode "slot_interval_start"
  encode SlotIntervalEnd = encode "slot_interval_end"
  encode (UseValue valueId) =
    encode
      { use_value: valueId
      }
  encode (Cond cond thenValue elseValue) =
    encode
      { if: cond
      , then: thenValue
      , else: elseValue
      }

instance decodeJsonValue :: Decode Value where
  decode a =
    ( ifM ((\x -> x == "slot_interval_start") <$> decode a)
        (pure SlotIntervalStart)
        (fail (ForeignError "Not \"slot_interval_start\" string"))
    )
      <|> ( \_ ->
            ifM ((\x -> x == "slot_interval_end") <$> decode a)
              (pure SlotIntervalEnd)
              (fail (ForeignError "Not \"slot_interval_end\" string"))
        )
      <|> ( \_ ->
            AvailableMoney <$> decodeProp "in_account" a
              <*> decodeProp "amount_of_token" a
        )
      <|> ( \_ ->
            Constant <$> decode a
        )
      <|> ( \_ ->
            NegValue <$> decodeProp "negate" a
        )
      <|> ( \_ ->
            AddValue <$> decodeProp "add" a
              <*> decodeProp "and" a
        )
      <|> ( \_ ->
            SubValue <$> decodeProp "value" a
              <*> decodeProp "minus" a
        )
      <|> ( \_ ->
            if (hasProperty "divide_by" a) then
              ( Scale
                  <$> ( Rational <$> decodeProp "times" a
                        <*> decodeProp "divide_by" a
                    )
                  <*> decodeProp "multiply" a
              )
            else
              ( MulValue <$> decodeProp "multiply" a
                  <*> decodeProp "times" a
              )
        )
      <|> ( \_ ->
            ChoiceValue <$> decodeProp "value_of_choice" a
        )
      <|> ( \_ ->
            UseValue <$> decodeProp "use_value" a
        )
      <|> ( \_ ->
            Cond <$> decodeProp "if" a
              <*> decodeProp "then" a
              <*> decodeProp "else" a
        )

instance showValue :: Show Value where
  show v = genericShow v

instance prettyValue :: Pretty Value where
  pretty v = genericPretty v

instance hasArgsValue :: Args Value where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
  | ValueGE Value Value
  | ValueGT Value Value
  | ValueLT Value Value
  | ValueLE Value Value
  | ValueEQ Value Value
  | TrueObs
  | FalseObs

derive instance genericObservation :: Generic Observation _

derive instance eqObservation :: Eq Observation

derive instance ordObservation :: Ord Observation

instance encodeJsonObservation :: Encode Observation where
  encode (AndObs lhs rhs) =
    encode
      { both: lhs
      , and: rhs
      }
  encode (OrObs lhs rhs) =
    encode
      { either: lhs
      , or: rhs
      }
  encode (NotObs obs) =
    encode
      { not: obs
      }
  encode (ChoseSomething choiceId) =
    encode
      { chose_something_for: choiceId
      }
  encode (ValueGE lhs rhs) =
    encode
      { value: lhs
      , ge_than: rhs
      }
  encode (ValueGT lhs rhs) =
    encode
      { value: lhs
      , gt: rhs
      }
  encode (ValueLT lhs rhs) =
    encode
      { value: lhs
      , lt: rhs
      }
  encode (ValueLE lhs rhs) =
    encode
      { value: lhs
      , le_than: rhs
      }
  encode (ValueEQ lhs rhs) =
    encode
      { value: lhs
      , equal_to: rhs
      }
  encode TrueObs = encode true
  encode FalseObs = encode false

instance decodeJsonObservation :: Decode Observation where
  decode a =
    ( ifM (decode a)
        (pure TrueObs)
        (fail (ForeignError "Not a boolean"))
    )
      <|> ( \_ ->
            ifM (not <$> decode a)
              (pure FalseObs)
              (fail (ForeignError "Not a boolean"))
        )
      <|> ( \_ ->
            AndObs <$> decodeProp "both" a
              <*> decodeProp "and" a
        )
      <|> ( \_ ->
            OrObs <$> decodeProp "either" a
              <*> decodeProp "or" a
        )
      <|> ( \_ ->
            NotObs <$> decodeProp "not" a
        )
      <|> ( \_ ->
            ChoseSomething <$> decodeProp "chose_something_for" a
        )
      <|> ( \_ ->
            ValueGE <$> decodeProp "value" a
              <*> decodeProp "ge_than" a
        )
      <|> ( \_ ->
            ValueGT <$> decodeProp "value" a
              <*> decodeProp "gt" a
        )
      <|> ( \_ ->
            ValueLT <$> decodeProp "value" a
              <*> decodeProp "lt" a
        )
      <|> ( \_ ->
            ValueLE <$> decodeProp "value" a
              <*> decodeProp "le_than" a
        )
      <|> ( \_ ->
            ValueEQ <$> decodeProp "value" a
              <*> decodeProp "equal_to" a
        )

instance showObservation :: Show Observation where
  show o = genericShow o

instance prettyObservation :: Pretty Observation where
  pretty v = genericPretty v

instance hasArgsObservation :: Args Observation where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

validInterval :: SlotInterval -> Boolean
validInterval (SlotInterval from to) = from <= to

above :: Slot -> SlotInterval -> Boolean
above v (SlotInterval _ to) = v > to

anyWithin :: forall f. Foldable f => Slot -> f SlotInterval -> Boolean
anyWithin v = any (\(SlotInterval from to) -> v >= from && v <= to)

data SlotInterval
  = SlotInterval Slot Slot

derive instance genericSlotInterval :: Generic SlotInterval _

derive instance eqSlotInterval :: Eq SlotInterval

derive instance ordSlotInterval :: Ord SlotInterval

instance showSlotInterval :: Show SlotInterval where
  show (SlotInterval from to) = "(Slot " <> show from <> " " <> show to <> ")"

instance genericEncodeSlotInterval :: Encode SlotInterval where
  encode a = genericEncode aesonCompatibleOptions a

instance genericDecodeSlotInterval :: Decode SlotInterval where
  decode a = genericDecode aesonCompatibleOptions a

ivFrom :: SlotInterval -> Slot
ivFrom (SlotInterval from _) = from

ivTo :: SlotInterval -> Slot
ivTo (SlotInterval _ to) = to

data Bound
  = Bound BigInteger BigInteger

derive instance genericBound :: Generic Bound _

derive instance eqBound :: Eq Bound

derive instance orBound :: Ord Bound

instance encodeJsonBound :: Encode Bound where
  encode (Bound fromSlot toSlot) =
    encode
      { from: fromSlot
      , to: toSlot
      }

instance decodeJsonBound :: Decode Bound where
  decode a =
    ( Bound <$> decodeProp "from" a
        <*> decodeProp "to" a
    )

instance showBound :: Show Bound where
  show = genericShow

instance prettyBound :: Pretty Bound where
  pretty v = genericPretty v

instance hasArgsBound :: Args Bound where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

getEncompassBound :: forall f. Foldable f => Functor f => f Bound -> Bound
getEncompassBound bounds =
  -- NOTE: We can't use the Min/Max semigroup with foldMap and we need to make a fromInt top/bottom
  --       because BigInteger doesn't have a Bounded instance. This should be fine in reality
  --       but it could lead to a bug if the lower bound is bigger than the biggest Int or if
  --       the highest bound is lower than the smallest Int.
  -- NOTE': We fold the datastructure twice instead of doing it in a single pass for simplicity
  --        in reality we shouldn't have many bounds, so it should be fine.
  let
    minBound = foldl min (fromInt top) $ map (\(Bound lower _) -> lower) bounds

    maxBound = foldl max (fromInt bottom) $ map (\(Bound _ higher) -> higher) bounds
  in
    Bound minBound maxBound

-- Possible actions that can be taken inside a `When` contract
data Action
  = {-
    Wait until `Party` makes a `Deposit` into `AccountId` of the ammount `Value` with `Token` currency.
  -} Deposit AccountId Party Token Value
  {-
    Wait for `ChoiceId _ Party` to take the named `ChoiceId String _` choice between the different Bound
  -}
  | Choice ChoiceId (Array Bound)
  | Notify Observation

derive instance genericAction :: Generic Action _

derive instance eqAction :: Eq Action

derive instance ordAction :: Ord Action

instance encodeJsonAction :: Encode Action where
  encode (Deposit accountId party token value) =
    encode
      { party: party
      , deposits: value
      , of_token: token
      , into_account: accountId
      }
  encode (Choice choiceId boundArray) =
    encode
      { choose_between: boundArray
      , for_choice: choiceId
      }
  encode (Notify obs) =
    encode
      { notify_if: obs
      }

instance decodeJsonAction :: Decode Action where
  decode a =
    ( Deposit <$> decodeProp "into_account" a
        <*> decodeProp "party" a
        <*> decodeProp "of_token" a
        <*> decodeProp "deposits" a
    )
      <|> ( \_ ->
            Choice <$> decodeProp "for_choice" a
              <*> decodeProp "choose_between" a
        )
      <|> ( \_ ->
            Notify <$> decodeProp "notify_if" a
        )

instance showAction :: Show Action where
  show (Choice cid bounds) = "(Choice " <> show cid <> " " <> show bounds <> ")"
  show v = genericShow v

instance prettyAction :: Pretty Action where
  pretty v = genericPretty v

instance hasArgsAction :: Args Action where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

data Payee
  = Account AccountId
  | Party Party

derive instance genericPayee :: Generic Payee _

derive instance eqPayee :: Eq Payee

derive instance ordPayee :: Ord Payee

instance encodeJsonPayee :: Encode Payee where
  encode (Account accountId) = encode { account: accountId }
  encode (Party party) = encode { party: party }

instance decodeJsonPayee :: Decode Payee where
  decode a =
    (Account <$> decodeProp "account" a)
      <|> ( \_ ->
            Party <$> decodeProp "party" a
        )

instance showPayee :: Show Payee where
  show v = genericShow v

instance prettyPayee :: Pretty Payee where
  pretty v = genericPretty v

instance hasArgsPayee :: Args Payee where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

data Case
  = Case Action Contract

derive instance genericCase :: Generic Case _

derive instance eqCase :: Eq Case

derive instance ordCase :: Ord Case

instance encodeJsonCase :: Encode Case where
  encode (Case action cont) =
    encode
      { case: action
      , then: cont
      }

instance decodeJsonCase :: Decode Case where
  decode a =
    ( Case <$> decodeProp "case" a
        <*> decodeProp "then" a
    )

instance showCase :: Show Case where
  show (Case action contract) = "Case " <> show action <> " " <> show contract

instance prettyCase :: Pretty Case where
  pretty v = genericPretty v

instance hasArgsCase :: Args Case where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

data Contract
  = Close
  | Pay AccountId Payee Token Value Contract
  | If Observation Contract Contract
  | When (Array Case) Timeout Contract
  | Let ValueId Value Contract
  | Assert Observation Contract

derive instance genericContract :: Generic Contract _

derive instance eqContract :: Eq Contract

derive instance ordContract :: Ord Contract

instance encodeJsonContract :: Encode Contract where
  encode Close = encode "close"
  encode (Pay accId payee token val cont) =
    encode
      { pay: val
      , token: token
      , from_account: accId
      , to: payee
      , then: cont
      }
  encode (If obs contTrue contFalse) =
    encode
      { if: obs
      , then: contTrue
      , else: contFalse
      }
  encode (When cases timeout cont) =
    encode
      { when: cases
      , timeout: timeout
      , timeout_continuation: cont
      }
  encode (Let valId val cont) =
    encode
      { let: valId
      , be: val
      , then: cont
      }
  encode (Assert obs cont) =
    encode
      { assert: obs
      , then: cont
      }

instance decodeJsonContract :: Decode Contract where
  decode a =
    ( ifM ((\x -> x == "close") <$> decode a)
        (pure Close)
        (fail (ForeignError "Not \"close\" string"))
    )
      <|> ( \_ ->
            Pay <$> decodeProp "from_account" a
              <*> decodeProp "to" a
              <*> decodeProp "token" a
              <*> decodeProp "pay" a
              <*> decodeProp "then" a
        )
      <|> ( \_ ->
            If <$> decodeProp "if" a
              <*> decodeProp "then" a
              <*> decodeProp "else" a
        )
      <|> ( \_ ->
            When <$> decodeProp "when" a
              <*> decodeProp "timeout" a
              <*> decodeProp "timeout_continuation" a
        )
      <|> ( \_ ->
            Let <$> decodeProp "let" a
              <*> decodeProp "be" a
              <*> decodeProp "then" a
        )
      <|> ( \_ ->
            Assert <$> decodeProp "assert" a
              <*> decodeProp "then" a
        )

instance showContract :: Show Contract where
  show v = genericShow v

instance prettyContract :: Pretty Contract where
  pretty v = genericPretty v

instance hasArgsContract :: Args Contract where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

newtype State
  = State
  { accounts :: Accounts
  , choices :: Map ChoiceId ChosenNum
  , boundValues :: Map ValueId BigInteger
  -- The minSlot is a lower bound for the current slot. When we are in the context of a Wallet or Dashboard
  -- we can just ask the time and calculate the current blockchain slot. But when we just have the context
  -- of a running contract we can't know the exact slot as transactions only provide an interval
  -- [lowSlot, highSlot].
  -- So the minSlot is the maximum number of the lowSlot we had so far, and we know that the current slot is
  -- higher or equal than that (because slots don't go back in time).
  -- The reason we keep track of it, is so that we can refine transaction intervals.
  -- If in a new transaction we have a lowSlot that is smaller than the minSlot, we can narrow the interval
  -- from [lowSlot, highSlot] to [minSlot, highSlot]
  , minSlot :: Slot
  }

derive instance genericState :: Generic State _

derive instance newtypeState :: Newtype State _

derive instance eqState :: Eq State

derive instance ordState :: Ord State

instance showState :: Show State where
  show v = genericShow v

instance encodeState :: Encode State where
  encode (State a) = encode a

instance decodeState :: Decode State where
  decode f = State <$> decode f

_accounts :: Lens' State (Accounts)
_accounts = _Newtype <<< prop (SProxy :: SProxy "accounts")

_choices :: Lens' State (Map ChoiceId ChosenNum)
_choices = _Newtype <<< prop (SProxy :: SProxy "choices")

_boundValues :: Lens' State (Map ValueId BigInteger)
_boundValues = _Newtype <<< prop (SProxy :: SProxy "boundValues")

_minSlot :: Lens' State Slot
_minSlot = _Newtype <<< prop (SProxy :: SProxy "minSlot")

newtype Environment
  = Environment { slotInterval :: SlotInterval }

derive instance genericEnvironment :: Generic Environment _

derive instance newtypeEnvironment :: Newtype Environment _

derive instance eqEnvironment :: Eq Environment

derive instance ordEnvironment :: Ord Environment

instance showEnvironment :: Show Environment where
  show v = genericShow v

_slotInterval :: Lens' Environment SlotInterval
_slotInterval = _Newtype <<< prop (SProxy :: SProxy "slotInterval")

makeEnvironment :: BigInteger -> BigInteger -> Environment
makeEnvironment l h = Environment { slotInterval: SlotInterval (Slot h) (Slot l) }

data Input
  = IDeposit AccountId Party Token BigInteger
  | IChoice ChoiceId ChosenNum
  | INotify

derive instance genericInput :: Generic Input _

derive instance eqInput :: Eq Input

derive instance ordInput :: Ord Input

instance showInput :: Show Input where
  show v = genericShow v

instance encodeJsonInput :: Encode Input where
  encode (IDeposit accId party tok amount) =
    encode
      { input_from_party: party
      , that_deposits: amount
      , of_token: tok
      , into_account: accId
      }
  encode (IChoice choiceId chosenNum) =
    encode
      { input_that_chooses_num: chosenNum
      , for_choice_id: choiceId
      }
  encode INotify = encode "input_notify"

instance decodeJsonInput :: Decode Input where
  decode a =
    ( ifM ((\x -> x == "input_notify") <$> decode a)
        (pure INotify)
        (fail (ForeignError "Not \"input_notify\" string"))
    )
      <|> ( \_ ->
            IDeposit <$> decodeProp "into_account" a
              <*> decodeProp "input_from_party" a
              <*> decodeProp "of_token" a
              <*> decodeProp "that_deposits" a
        )
      <|> ( \_ ->
            IChoice <$> decodeProp "for_choice_id" a
              <*> decodeProp "input_that_chooses_num" a
        )

-- Processing of slot interval
data IntervalError
  = InvalidInterval SlotInterval
  | IntervalInPastError Slot SlotInterval

derive instance genericIntervalError :: Generic IntervalError _

derive instance eqIntervalError :: Eq IntervalError

derive instance ordIntervalError :: Ord IntervalError

instance showIntervalError :: Show IntervalError where
  show (InvalidInterval interval) = "Invalid interval: " <> show interval
  show (IntervalInPastError slot interval) = "Interval is in the past, the current slot is " <> show slot <> " but the interval is " <> show interval

instance genericEncodeIntervalError :: Encode IntervalError where
  encode a = genericEncode aesonCompatibleOptions a

instance genericDecodeIntervalError :: Decode IntervalError where
  decode a = genericDecode aesonCompatibleOptions a

data IntervalResult
  = IntervalTrimmed Environment State
  | IntervalError IntervalError

derive instance genericIntervalResult :: Generic IntervalResult _

derive instance eqIntervalResult :: Eq IntervalResult

derive instance ordIntervalResult :: Ord IntervalResult

instance showIntervalResult :: Show IntervalResult where
  show v = genericShow v

data Payment
  = Payment AccountId Payee Money

derive instance genericPayment :: Generic Payment _

derive instance eqPayment :: Eq Payment

derive instance ordPayment :: Ord Payment

instance showPayment :: Show Payment where
  show = genericShow

instance encodePayment :: Encode Payment where
  encode a = genericEncode aesonCompatibleOptions a

instance decodePayment :: Decode Payment where
  decode a = genericDecode aesonCompatibleOptions a

data ReduceEffect
  = ReduceWithPayment Payment
  | ReduceNoPayment

derive instance genericReduceEffect :: Generic ReduceEffect _

derive instance eqReduceEffect :: Eq ReduceEffect

instance showReduceEffect :: Show ReduceEffect where
  show = genericShow

data ReduceWarning
  = ReduceNoWarning
  | ReduceNonPositivePay AccountId Payee Token BigInteger
  ---------------------- v src v dest v paid v expected
  | ReducePartialPay AccountId Payee Token BigInteger BigInteger
  -------------------------- v oldVal  v newVal
  | ReduceShadowing ValueId BigInteger BigInteger
  | ReduceAssertionFailed

derive instance genericReduceWarning :: Generic ReduceWarning _

derive instance eqReduceWarning :: Eq ReduceWarning

derive instance ordReduceWarning :: Ord ReduceWarning

instance showReduceWarning :: Show ReduceWarning where
  show = genericShow

data ReduceStepResult
  = Reduced ReduceWarning ReduceEffect State Contract
  | NotReduced
  | AmbiguousSlotIntervalReductionError

derive instance genericReduceStepResult :: Generic ReduceStepResult _

derive instance eqReduceStepResult :: Eq ReduceStepResult

instance showReduceStepResult :: Show ReduceStepResult where
  show = genericShow

data ReduceResult
  = ContractQuiescent Boolean (List ReduceWarning) (List Payment) State Contract
  | RRAmbiguousSlotIntervalError

derive instance genericReduceResult :: Generic ReduceResult _

derive instance eqReduceResult :: Eq ReduceResult

instance showReduceResult :: Show ReduceResult where
  show = genericShow

data ApplyWarning
  = ApplyNoWarning
  | ApplyNonPositiveDeposit Party AccountId Token BigInteger

derive instance genericApplyWarning :: Generic ApplyWarning _

derive instance eqApplyWarning :: Eq ApplyWarning

derive instance ordApplyWarning :: Ord ApplyWarning

instance showApplyWarning :: Show ApplyWarning where
  show = genericShow

data ApplyResult
  = Applied ApplyWarning State Contract
  | ApplyNoMatchError

derive instance genericApplyResult :: Generic ApplyResult _

derive instance eqApplyResult :: Eq ApplyResult

derive instance ordApplyResult :: Ord ApplyResult

instance showApplyResult :: Show ApplyResult where
  show = genericShow

data ApplyAllResult
  = ApplyAllSuccess Boolean (List TransactionWarning) (List Payment) State Contract
  | ApplyAllNoMatchError
  | ApplyAllAmbiguousSlotIntervalError

derive instance genericApplyAllResult :: Generic ApplyAllResult _

derive instance eqApplyAllResult :: Eq ApplyAllResult

instance showApplyAllResult :: Show ApplyAllResult where
  show = genericShow

data TransactionWarning
  = TransactionNonPositiveDeposit Party AccountId Token BigInteger
  | TransactionNonPositivePay AccountId Payee Token BigInteger
  | TransactionPartialPay AccountId Payee Token BigInteger BigInteger
  --                         ^ src    ^ dest       ^ paid     ^ expected
  | TransactionShadowing ValueId BigInteger BigInteger
  --                           oldVal ^  newVal ^
  | TransactionAssertionFailed

derive instance genericTransactionWarning :: Generic TransactionWarning _

derive instance eqTransactionWarning :: Eq TransactionWarning

derive instance ordTransactionWarning :: Ord TransactionWarning

instance showTransactionWarning :: Show TransactionWarning where
  show = genericShow

instance encodeTransactionWarning :: Encode TransactionWarning where
  encode TransactionAssertionFailed = encode "assertion_failed"
  encode (TransactionNonPositiveDeposit party accId tok amount) =
    encode
      { party: party
      , asked_to_deposit: amount
      , of_token: tok
      , in_account: accId
      }
  encode (TransactionNonPositivePay accId payee tok amount) =
    encode
      { account: accId
      , asked_to_pay: amount
      , of_token: tok
      , to_payee: payee
      }
  encode (TransactionPartialPay accId payee tok paid expected) =
    encode
      { account: accId
      , asked_to_pay: expected
      , of_token: tok
      , to_payee: payee
      , but_only_paid: paid
      }
  encode (TransactionShadowing valId oldVal newVal) =
    encode
      { value_id: valId
      , had_value: oldVal
      , is_now_assigned: newVal
      }

instance decodeTransactionWarning :: Decode TransactionWarning where
  decode a =
    ( ifM ((\x -> x == "assertion_failed") <$> decode a)
        (pure TransactionAssertionFailed)
        (fail (ForeignError "Not \"assertion_failed\" string"))
    )
      <|> ( \_ ->
            TransactionNonPositiveDeposit <$> decodeProp "party" a
              <*> decodeProp "in_account" a
              <*> decodeProp "of_token" a
              <*> decodeProp "asked_to_deposit" a
        )
      <|> ( \_ ->
            if (hasProperty "but_only_paid" a) then
              ( TransactionPartialPay <$> decodeProp "account" a
                  <*> decodeProp "to_payee" a
                  <*> decodeProp "of_token" a
                  <*> decodeProp "but_only_paid" a
                  <*> decodeProp "asked_to_pay" a
              )
            else
              ( TransactionNonPositivePay <$> decodeProp "account" a
                  <*> decodeProp "to_payee" a
                  <*> decodeProp "of_token" a
                  <*> decodeProp "asked_to_pay" a
              )
        )
      <|> ( \_ ->
            TransactionShadowing <$> decodeProp "value_id" a
              <*> decodeProp "had_value" a
              <*> decodeProp "is_now_assigned" a
        )

-- | Transaction error
data TransactionError
  = TEAmbiguousSlotIntervalError
  | TEApplyNoMatchError
  | TEIntervalError IntervalError
  | TEUselessTransaction

derive instance genericTransactionError :: Generic TransactionError _

derive instance eqTransactionError :: Eq TransactionError

derive instance ordTransactionError :: Ord TransactionError

instance showTransactionError :: Show TransactionError where
  show TEAmbiguousSlotIntervalError = "Abiguous slot interval"
  show TEApplyNoMatchError = "At least one of the inputs in the transaction is not allowed by the contract"
  show (TEIntervalError err) = show err
  show TEUselessTransaction = "Useless Transaction"

instance genericEncodeTransactionError :: Encode TransactionError where
  encode a = genericEncode aesonCompatibleOptions a

instance genericDecodeTransactionError :: Decode TransactionError where
  decode a = genericDecode aesonCompatibleOptions a

newtype TransactionInput
  = TransactionInput
  { interval :: SlotInterval
  , inputs :: (List Input)
  }

derive instance genericTransactionInput :: Generic TransactionInput _

derive instance newtypeTransactionInput :: Newtype TransactionInput _

derive instance eqTransactionInput :: Eq TransactionInput

derive instance ordTransactionInput :: Ord TransactionInput

instance showTransactionInput :: Show TransactionInput where
  show = genericShow

instance encodeTransactionInput :: Encode TransactionInput where
  encode ( TransactionInput
      { interval: (SlotInterval (Slot fromSlot) (Slot toSlot))
    , inputs: txInps
    }
  ) =
    encode
      { tx_interval:
          { from: fromSlot
          , to: toSlot
          }
      , tx_inputs: txInps
      }

instance decodeTransactionInput :: Decode TransactionInput where
  decode a = do
    interv <- decode =<< readProp "tx_interval" a
    fromSlot <- decode =<< readProp "from" interv
    toSlot <- decode =<< readProp "to" interv
    inputs <- decode =<< readProp "tx_inputs" a
    pure
      ( TransactionInput
          { interval: (SlotInterval (Slot fromSlot) (Slot toSlot))
          , inputs: inputs
          }
      )

data TransactionOutput
  = TransactionOutput
    { txOutWarnings :: List TransactionWarning
    , txOutPayments :: List Payment
    , txOutState :: State
    , txOutContract :: Contract
    }
  | Error TransactionError

derive instance genericTransactionOutput :: Generic TransactionOutput _

derive instance eqTransactionOutput :: Eq TransactionOutput

instance showTransactionOutput :: Show TransactionOutput where
  show = genericShow

emptyState :: Slot -> State
emptyState sn =
  State
    { accounts: mempty
    , choices: mempty
    , boundValues: mempty
    , minSlot: sn
    }

inBounds :: ChosenNum -> Array Bound -> Boolean
inBounds num = any (\(Bound l u) -> num >= l && num <= u)

boundFrom :: Bound -> BigInteger
boundFrom (Bound from _) = from

boundTo :: Bound -> BigInteger
boundTo (Bound _ to) = to

-- Note: We use guards here because currently nested ifs break purty formatting
--       We need to upgrade purty and purescript to fix
fixInterval :: SlotInterval -> State -> IntervalResult
fixInterval interval@(SlotInterval from to) (State state)
  | (not <<< validInterval) interval = IntervalError (InvalidInterval interval)
  | state.minSlot `above` interval = IntervalError (IntervalInPastError state.minSlot interval)
  | otherwise =
    let
      -- newLow is both new "low" and new "minSlot" (the lower bound for slotNum)
      newLow = max from state.minSlot

      -- We know high is greater or equal than newLow (prove)
      currentInterval = SlotInterval newLow to

      env = Environment { slotInterval: currentInterval }

      newState = State (state { minSlot = newLow })
    in
      IntervalTrimmed env newState

-- EVALUATION
-- | Evaluate a @Value@ to Integer
evalValue :: Environment -> State -> Value -> BigInteger
evalValue env state value =
  let
    eval = evalValue env state
  in
    case value of
      AvailableMoney accId token -> moneyInAccount accId token (unwrap state).accounts
      Constant integer -> integer
      NegValue val -> negate (eval val)
      AddValue lhs rhs -> eval lhs + eval rhs
      SubValue lhs rhs -> eval lhs - eval rhs
      MulValue lhs rhs -> eval lhs * eval rhs
      Scale (Rational n d) rhs ->
        let
          nn = eval rhs * n

          q = nn `div` d

          r = nn `mod` d
        in
          if abs r * fromInt 2 < abs d then q else q + signum nn * signum d
      ChoiceValue choiceId -> fromMaybe zero $ Map.lookup choiceId (unwrap state).choices
      SlotIntervalStart -> view (_slotInterval <<< to ivFrom <<< to unwrap) env
      SlotIntervalEnd -> view (_slotInterval <<< to ivTo <<< to unwrap) env
      UseValue valId -> fromMaybe zero $ Map.lookup valId (unwrap state).boundValues
      Cond cond thn els -> if evalObservation env state cond then eval thn else eval els

-- | Evaluate an @Observation@ to Bool
evalObservation :: Environment -> State -> Observation -> Boolean
evalObservation env state obs =
  let
    evalObs = evalObservation env state

    evalVal = evalValue env state
  in
    case obs of
      AndObs lhs rhs -> evalObs lhs && evalObs rhs
      OrObs lhs rhs -> evalObs lhs || evalObs rhs
      NotObs subObs -> not (evalObs subObs)
      ChoseSomething choiceId -> choiceId `Map.member` (unwrap state).choices
      ValueGE lhs rhs -> evalVal lhs >= evalVal rhs
      ValueGT lhs rhs -> evalVal lhs > evalVal rhs
      ValueLT lhs rhs -> evalVal lhs < evalVal rhs
      ValueLE lhs rhs -> evalVal lhs <= evalVal rhs
      ValueEQ lhs rhs -> evalVal lhs == evalVal rhs
      TrueObs -> true
      FalseObs -> false

asset :: CurrencySymbol -> TokenName -> BigInteger -> Assets
asset cur tok balance = Assets (Map.singleton cur (Map.singleton tok balance))

-- | Pick the first account with money in it
refundOne :: Accounts -> Maybe (Tuple (Tuple Party Money) Accounts)
refundOne accounts = case Map.toUnfoldable accounts of
  Nil -> Nothing
  Tuple (Tuple accId (Token cur tok)) balance : rest ->
    if balance > zero then
      Just (Tuple (Tuple accId (asset cur tok balance)) (Map.fromFoldable rest))
    else
      refundOne (Map.fromFoldable rest)

-- | Obtains the amount of money available an account
moneyInAccount :: AccountId -> Token -> Accounts -> BigInteger
moneyInAccount accId token accounts = fromMaybe zero (Map.lookup (Tuple accId token) accounts)

-- | Sets the amount of money available in an account
updateMoneyInAccount :: AccountId -> Token -> BigInteger -> Accounts -> Accounts
updateMoneyInAccount accId token amount = if amount <= zero then Map.delete (Tuple accId token) else Map.insert (Tuple accId token) amount

{-| Add the given amount of money to an account (only if it is positive).
    Return the updated Map
-}
addMoneyToAccount :: AccountId -> Token -> BigInteger -> Accounts -> Accounts
addMoneyToAccount accId token amount accounts =
  let
    balance = moneyInAccount accId token accounts

    newBalance = balance + amount
  in
    if amount <= zero then
      accounts
    else
      updateMoneyInAccount accId token newBalance accounts

{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
giveMoney :: AccountId -> Payee -> Token -> BigInteger -> Accounts -> Tuple ReduceEffect Accounts
giveMoney accountId payee token@(Token cur tok) amount accounts =
  let
    newAccounts = case payee of
      Party _ -> accounts
      Account accId -> addMoneyToAccount accId token amount accounts
  in
    Tuple (ReduceWithPayment (Payment accountId payee (asset cur tok amount))) accounts

-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of
  Close -> case refundOne (unwrap state).accounts of
    Just (Tuple (Tuple party money) newAccounts) ->
      let
        oldState = unwrap state

        newState = wrap (oldState { accounts = newAccounts })
      in
        Reduced ReduceNoWarning (ReduceWithPayment (Payment party (Party party) money)) newState Close
    Nothing -> NotReduced
  Pay accId payee tok val cont ->
    let
      amountToPay = evalValue env state val
    in
      if amountToPay <= zero then
        let
          warning = ReduceNonPositivePay accId payee tok amountToPay
        in
          Reduced warning ReduceNoPayment state cont
      else
        let
          balance = moneyInAccount accId tok (unwrap state).accounts

          paidAmount = min balance amountToPay

          newBalance = balance - paidAmount

          newAccs = updateMoneyInAccount accId tok newBalance (unwrap state).accounts

          warning =
            if paidAmount < amountToPay then
              ReducePartialPay accId payee tok paidAmount amountToPay
            else
              ReduceNoWarning

          (Tuple payment finalAccs) = giveMoney accId payee tok paidAmount newAccs

          newState = wrap ((unwrap state) { accounts = finalAccs })
        in
          Reduced warning payment newState cont
  If obs cont1 cont2 ->
    let
      cont = if evalObservation env state obs then cont1 else cont2
    in
      Reduced ReduceNoWarning ReduceNoPayment state cont
  When _ timeout nextContract ->
    let
      startSlot = view (_slotInterval <<< to ivFrom) env

      endSlot = view (_slotInterval <<< to ivTo) env
    in
      if endSlot < timeout then
        NotReduced
      else
        if timeout <= startSlot then
          Reduced ReduceNoWarning ReduceNoPayment state nextContract
        else
          AmbiguousSlotIntervalReductionError
  Let valId val nextContract ->
    let
      evaluatedValue = evalValue env state val

      newState = over _boundValues (Map.insert valId evaluatedValue) state

      warn = case Map.lookup valId (unwrap state).boundValues of
        Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
        Nothing -> ReduceNoWarning
    in
      Reduced warn ReduceNoPayment newState nextContract
  Assert obs cont ->
    let
      warning =
        if evalObservation env state obs then
          ReduceNoWarning
        else
          ReduceAssertionFailed
    in
      Reduced warning ReduceNoPayment state cont

-- | Reduce a contract until it cannot be reduced more
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent startEnv startState startContract =
  let
    reductionLoop ::
      Boolean -> Environment -> State -> Contract -> (List ReduceWarning) -> (List Payment) -> ReduceResult
    reductionLoop reduced env state contract warnings payments = case reduceContractStep env state contract of
      Reduced warning effect newState nextContract ->
        let
          newWarnings = if warning == ReduceNoWarning then warnings else warning : warnings

          newPayments = case effect of
            ReduceWithPayment payment -> payment : payments
            ReduceNoPayment -> payments
        in
          reductionLoop true env newState nextContract newWarnings newPayments
      AmbiguousSlotIntervalReductionError -> RRAmbiguousSlotIntervalError
      -- this is the last invocation of reductionLoop, so we can reverse lists
      NotReduced -> ContractQuiescent reduced (reverse warnings) (reverse payments) state contract
  in
    reductionLoop false startEnv startState startContract mempty mempty

applyCases :: Environment -> State -> Input -> List Case -> ApplyResult
applyCases env state input cases = case input, cases of
  IDeposit accId1 party1 tok1 amount, (Case (Deposit accId2 party2 tok2 val) cont) : rest ->
    if accId1 == accId2 && party1 == party2 && tok1 == tok2
      && amount
      == evalValue env state val then
      let
        warning =
          if amount > zero then
            ApplyNoWarning
          else
            ApplyNonPositiveDeposit party2 accId2 tok2 amount

        newAccounts = addMoneyToAccount accId1 tok1 amount (unwrap state).accounts

        newState = wrap ((unwrap state) { accounts = newAccounts })
      in
        Applied warning newState cont
    else
      applyCases env state input rest
  IChoice choId1 choice, (Case (Choice choId2 bounds) cont) : rest ->
    let
      newState = over _choices (Map.insert choId1 choice) state
    in
      if choId1 == choId2 && inBounds choice bounds then
        Applied ApplyNoWarning newState cont
      else
        applyCases env state input rest
  INotify, (Case (Notify obs) cont) : _
    | evalObservation env state obs -> Applied ApplyNoWarning state cont
  _, _ : rest -> applyCases env state input rest
  _, Nil -> ApplyNoMatchError

applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput env state input (When cases _ _) = applyCases env state input (fromFoldable cases)

applyInput _ _ _ _ = ApplyNoMatchError

convertReduceWarnings :: List ReduceWarning -> List TransactionWarning
convertReduceWarnings Nil = Nil

convertReduceWarnings (first : rest) =
  ( case first of
      ReduceNoWarning -> Nil
      ReduceNonPositivePay accId payee tok amount -> (TransactionNonPositivePay accId payee tok amount) : Nil
      ReducePartialPay accId payee tok paid expected -> (TransactionPartialPay accId payee tok paid expected) : Nil
      ReduceShadowing valId oldVal newVal -> (TransactionShadowing valId oldVal newVal) : Nil
      ReduceAssertionFailed -> TransactionAssertionFailed : Nil
  )
    <> convertReduceWarnings rest

convertApplyWarning :: ApplyWarning -> List TransactionWarning
convertApplyWarning warn = case warn of
  ApplyNoWarning -> Nil
  ApplyNonPositiveDeposit party accId tok amount -> (TransactionNonPositiveDeposit party accId tok amount) : Nil

-- | Apply a list of Inputs to the contract
applyAllInputs :: Environment -> State -> Contract -> (List Input) -> ApplyAllResult
applyAllInputs startEnv startState startContract startInputs =
  let
    applyAllLoop ::
      Boolean ->
      Environment ->
      State ->
      Contract ->
      List Input ->
      List TransactionWarning ->
      List Payment ->
      ApplyAllResult
    applyAllLoop contractChanged env state contract inputs warnings payments = case reduceContractUntilQuiescent env state contract of
      RRAmbiguousSlotIntervalError -> ApplyAllAmbiguousSlotIntervalError
      ContractQuiescent reduced reduceWarns pays curState cont -> case inputs of
        Nil ->
          ApplyAllSuccess (contractChanged || reduced)
            (warnings <> (convertReduceWarnings reduceWarns))
            (payments <> pays)
            curState
            cont
        (input : rest) -> case applyInput env curState input cont of
          Applied applyWarn newState nextContract ->
            applyAllLoop true env newState nextContract rest
              ( warnings <> (convertReduceWarnings reduceWarns)
                  <> (convertApplyWarning applyWarn)
              )
              (payments <> pays)
          ApplyNoMatchError -> ApplyAllNoMatchError
  in
    applyAllLoop false startEnv startState startContract startInputs mempty mempty

isClose :: Contract -> Boolean
isClose Close = true

isClose _ = false

-- | Try to compute outputs of a transaction give its input
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract =
  let
    inputs = (unwrap tx).inputs
  in
    case fixInterval (unwrap tx).interval state of
      IntervalTrimmed env fixState -> case applyAllInputs env fixState contract inputs of
        ApplyAllSuccess reduced warnings payments newState cont ->
          if not reduced && (not (isClose contract) || (Map.isEmpty $ (unwrap state).accounts)) then
            Error TEUselessTransaction
          else
            TransactionOutput
              { txOutWarnings: warnings
              , txOutPayments: payments
              , txOutState: newState
              , txOutContract: cont
              }
        ApplyAllNoMatchError -> Error TEApplyNoMatchError
        ApplyAllAmbiguousSlotIntervalError -> Error TEAmbiguousSlotIntervalError
      IntervalError error -> Error (TEIntervalError error)

moneyInContract :: State -> Money
moneyInContract state =
  foldMapWithIndex
    (\(Tuple _ (Token cur tok)) balance -> asset cur tok balance)
    (unwrap state).accounts

newtype Timeouts
  = Timeouts { maxTime :: Timeout, minTime :: Maybe Timeout }

derive instance newtypeTimeouts :: Newtype Timeouts _

-- The eq and show instances are only needed for quickcheck
derive newtype instance eqTimeouts :: Eq Timeouts

derive newtype instance showTimeouts :: Show Timeouts

class HasTimeout a where
  timeouts :: a -> Timeouts

instance hasTimeoutContract :: HasTimeout Contract where
  timeouts Close = Timeouts { maxTime: zero, minTime: Nothing }
  timeouts (Pay _ _ _ _ contract) = timeouts contract
  timeouts (If _ contractTrue contractFalse) = timeouts [ contractTrue, contractFalse ]
  timeouts (When cases timeout contract) =
    timeouts
      [ timeouts cases
      , Timeouts { maxTime: timeout, minTime: Just timeout }
      , timeouts contract
      ]
  timeouts (Let _ _ contract) = timeouts contract
  timeouts (Assert _ contract) = timeouts contract

instance hasTimeoutCase :: HasTimeout Case where
  timeouts (Case _ contract) = timeouts contract

instance hasTimeoutArrayOfTimeouts :: HasTimeout (Array Timeouts) where
  timeouts ts =
    Timeouts
      { maxTime: maxOf (map (_.maxTime <<< unwrap) ts)
      , minTime: minOf (map (_.minTime <<< unwrap) ts)
      }
else instance hasTimeoutArray :: HasTimeout a => HasTimeout (Array a) where
  timeouts vs = timeouts $ map timeouts vs

maxOf :: Array Timeout -> Timeout
maxOf = foldl max zero

minOf :: Array (Maybe Timeout) -> Maybe Timeout
minOf as = minimum $ catMaybes as

aesonCompatibleOptions :: Options
aesonCompatibleOptions =
  defaultOptions
    { unwrapSingleConstructors = true
    , sumEncoding = aesonSumEncoding
    }
