module Marlowe.ActusBlockly where

import Prelude
import Blockly.Generator (Generator, getFieldValue, getType, insertGeneratorFunction, mkGenerator, statementToCode)
import Blockly.Internal (AlignDirection(..), Arg(..), BlockDefinition(..), block, blockType, defaultBlockDefinition, style, x, xml, y)
import Blockly.Types (Block, Blockly)
import Blockly.Toolbox (Toolbox(..), category, leaf)
import Control.Alternative ((<|>))
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap, rmap)
import Data.BigInteger (BigInteger)
import Data.BigInteger as BigInteger
import Data.Date (exactDate)
import Data.Either (Either)
import Data.Either as Either
import Data.Enum (class BoundedEnum, class Enum, upFromIncluding, toEnum)
import Data.FloatParser (parseFloat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence, traverse_)
import Effect (Effect)
import Foreign (F)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (genericEncode, genericDecode, encodeJSON)
import Foreign.Generic.Class (Options, defaultOptions, aesonSumEncoding)
import Foreign.JSON (parseJSON)
import Halogen.HTML (HTML)
import Halogen.HTML.Properties (id_)
import Language.Marlowe.ACTUS.Definitions.ContractTerms (Assertion(..), AssertionContext(..), Assertions(..), BDC(..), CR(..), PRF(..), ContractTerms(..), CT(..), Cycle(..), DCC(..), EOMC(..), FEB(..), PPEF(..), PYTP(..), Period(..), SCEF(..), ScheduleConfig(..), Stub(..), IPCB(..))
import Record (merge)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Basic (parens, runParser')

rootBlockName :: String
rootBlockName = "root_contract"

data ActusContractType
  = PaymentAtMaturity
  | LinearAmortizer
  | NegativeAmortizer

derive instance actusContractType :: Generic ActusContractType _

instance showActusContractType :: Show ActusContractType where
  show = genericShow

instance eqActusContractType :: Eq ActusContractType where
  eq = genericEq

instance ordActusContractType :: Ord ActusContractType where
  compare = genericCompare

instance enumActusContractType :: Enum ActusContractType where
  succ = genericSucc
  pred = genericPred

instance boundedActusContractType :: Bounded ActusContractType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumActusContractType :: BoundedEnum ActusContractType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

actusContractTypes :: Array ActusContractType
actusContractTypes = upFromIncluding bottom

data ActusValueType
  = ActusDate
  | ActusCycleType
  | ActusDecimalType
  | ActusIntegerType
  | ActusAssertionContextType
  | ActusAssertionType

derive instance actusActusValueType :: Generic ActusValueType _

instance showActusValueType :: Show ActusValueType where
  show = genericShow

instance eqActusValueType :: Eq ActusValueType where
  eq = genericEq

instance ordActusValueType :: Ord ActusValueType where
  compare = genericCompare

instance enumActusValueType :: Enum ActusValueType where
  succ = genericSucc
  pred = genericPred

instance boundedActusValueType :: Bounded ActusValueType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumActusValueType :: BoundedEnum ActusValueType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

actusValueTypes :: Array ActusValueType
actusValueTypes = upFromIncluding bottom

data ActusPeriodType
  = PeriodDayType
  | PeriodMonthType
  | PeriodQuarterType
  | PeriodYearType

derive instance actusActusPeriodType :: Generic ActusPeriodType _

instance eqActusPeriodType :: Eq ActusPeriodType where
  eq = genericEq

instance ordActusPeriodType :: Ord ActusPeriodType where
  compare = genericCompare

instance enumActusPeriodType :: Enum ActusPeriodType where
  succ = genericSucc
  pred = genericPred

instance boundedActusPeriodType :: Bounded ActusPeriodType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumActusPeriodType :: BoundedEnum ActusPeriodType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showActusPeriod :: Show ActusPeriodType where
  show = encodeJSON

instance encodeJsonActusPeriod :: Encode ActusPeriodType where
  encode a = genericEncode aesonCompatibleOptions a

instance decodeJsonActusPeriod :: Decode ActusPeriodType where
  decode a = genericDecode aesonCompatibleOptions a

actusPeriodTypes :: Array ActusPeriodType
actusPeriodTypes = upFromIncluding bottom

data BlockType
  = BaseContractType
  | ActusContractType ActusContractType
  | ActusValueType ActusValueType
  | ActusPeriodType ActusPeriodType

derive instance genericBlockType :: Generic BlockType _

instance eqBlockType :: Eq BlockType where
  eq = genericEq

instance ordBlockType :: Ord BlockType where
  compare = genericCompare

instance enumBlockType :: Enum BlockType where
  succ = genericSucc
  pred = genericPred

instance boundedBlockType :: Bounded BlockType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumBlockType :: BoundedEnum BlockType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showBlockType :: Show BlockType where
  show BaseContractType = "BaseContractType"
  show (ActusContractType c) = show c
  show (ActusValueType c) = show c
  show (ActusPeriodType c) = show c

contractColour :: String
contractColour = "#a380bc"

actusColour :: String
actusColour = "#1a7b84"

valueColour :: String
valueColour = "#eb2256"

periodColour :: String
periodColour = "#e6aa00"

blockColour :: BlockType -> String
blockColour BaseContractType = contractColour

blockColour (ActusContractType _) = actusColour

blockColour (ActusValueType _) = valueColour

blockColour (ActusPeriodType _) = periodColour

blockDefinitions :: Array BlockDefinition
blockDefinitions = map toDefinition (upFromIncluding bottom)

toDefinition :: BlockType -> BlockDefinition
toDefinition BaseContractType =
  BlockDefinition
    $ merge
        { type: show BaseContractType
        , message0: "%1 CONTRACT %2 %3 %4 %5"
        , args0:
            [ DummyRight
            , DummyRight
            , DummyRight
            , Statement { name: (show BaseContractType), check: (show BaseContractType), align: Right }
            , DummyRight
            ]
        , colour: blockColour BaseContractType
        , inputsInline: Just false
        }
        defaultBlockDefinition

toDefinition (ActusContractType PaymentAtMaturity) =
  BlockDefinition
    $ merge
        { type: show PaymentAtMaturity
        , message0:
            "Principal At Maturity %1"
              <> "start date * %2"
              <> "maturity date * %3"
              <> "notional * %4"
              <> "premium/discount %5"
              <> "interest rate %6"
              <> "purchase date %7"
              <> "purchase price %8"
              <> "initial exchange date %9"
              <> "termination date %10"
              <> "termination price %11"
              <> "rate reset cycle %12"
              <> "interest payment cycle %13"
              <> "observation constraints %14"
              <> "payoff analysis constraints %15"
        -- Any collateral-related code is commented out, until implemented properly
        -- <> "collateral amount %16"
        , args0:
            [ DummyCentre
            , Value { name: "start_date", check: "date", align: Right }
            , Value { name: "maturity_date", check: "date", align: Right }
            , Value { name: "notional", check: "decimal", align: Right }
            , Value { name: "premium_discount", check: "decimal", align: Right }
            , Value { name: "interest_rate", check: "decimal", align: Right }
            , Value { name: "purchase_date", check: "date", align: Right }
            , Value { name: "purchase_price", check: "decimal", align: Right }
            , Value { name: "initial_exchange_date", check: "date", align: Right }
            , Value { name: "termination_date", check: "date", align: Right }
            , Value { name: "termination_price", check: "decimal", align: Right }
            , Value { name: "rate_reset_cycle", check: "cycle", align: Right }
            , Value { name: "interest_rate_cycle", check: "cycle", align: Right }
            , Value { name: "interest_rate_ctr", check: "assertionCtx", align: Right }
            , Value { name: "payoff_ctr", check: "assertion", align: Right }
            -- Any collateral-related code is commented out, until implemented properly
            -- , Value { name: "collateral", check: "integer", align: Right }
            ]
        , colour: blockColour (ActusContractType PaymentAtMaturity)
        , previousStatement: Just (show BaseContractType)
        , inputsInline: Just false
        }
        defaultBlockDefinition

toDefinition (ActusContractType LinearAmortizer) =
  BlockDefinition
    $ merge
        { type: show LinearAmortizer
        , message0:
            "Linear Amortizer %1"
              <> "start date * %2"
              <> "maturity date %3"
              <> "notional * %4"
              <> "premium/discount %5"
              <> "interest rate * %6"
              <> "purchase date %7"
              <> "purchase price %8"
              <> "initial exchange date %9"
              <> "termination date %10"
              <> "termination price %11"
              <> "periodic payment amount %12"
              <> "rate reset cycle %13"
              <> "interest payment cycle %14"
              <> "principal redemption cycle * %15"
              <> "observation constraints %16"
              <> "payoff analysis constraints %17"
        -- Any collateral-related code is commented out, until implemented properly
        -- <> "collateral amount %18"
        , args0:
            [ DummyCentre
            , Value { name: "start_date", check: "date", align: Right }
            , Value { name: "maturity_date", check: "date", align: Right }
            , Value { name: "notional", check: "decimal", align: Right }
            , Value { name: "premium_discount", check: "decimal", align: Right }
            , Value { name: "interest_rate", check: "decimal", align: Right }
            , Value { name: "purchase_date", check: "date", align: Right }
            , Value { name: "purchase_price", check: "decimal", align: Right }
            , Value { name: "initial_exchange_date", check: "date", align: Right }
            , Value { name: "termination_date", check: "date", align: Right }
            , Value { name: "termination_price", check: "decimal", align: Right }
            , Value { name: "periodic_payment_amount", check: "decimal", align: Right }
            , Value { name: "rate_reset_cycle", check: "cycle", align: Right }
            , Value { name: "interest_rate_cycle", check: "cycle", align: Right }
            , Value { name: "principal_redemption_cycle", check: "cycle", align: Right }
            , Value { name: "interest_rate_ctr", check: "assertionCtx", align: Right }
            , Value { name: "payoff_ctr", check: "assertion", align: Right }
            -- Any collateral-related code is commented out, until implemented properly
            -- , Value { name: "collateral", check: "integer", align: Right }
            ]
        , colour: blockColour (ActusContractType LinearAmortizer)
        , previousStatement: Just (show BaseContractType)
        , inputsInline: Just false
        }
        defaultBlockDefinition

toDefinition (ActusContractType NegativeAmortizer) =
  BlockDefinition
    $ merge
        { type: show NegativeAmortizer
        , message0:
            "Negative Amortizer %1"
              <> "start date * %2"
              <> "maturity date %3"
              <> "notional %4"
              <> "premium/discount * %5"
              <> "interest rate * %6"
              <> "purchase date %7"
              <> "purchase price %8"
              <> "initial exchange date * %9"
              <> "termination date %10"
              <> "termination price %11"
              <> "periodic payment amount %12"
              <> "rate reset cycle %13"
              <> "interest payment cycle %14"
              <> "principal redemption cycle %15"
              <> "observation constraints %16"
              <> "payoff analysis constraints %17"
        -- Any collateral-related code is commented out, until implemented properly
        -- <> "collateral amount %18"
        , args0:
            [ DummyCentre
            , Value { name: "start_date", check: "date", align: Right }
            , Value { name: "maturity_date", check: "date", align: Right }
            , Value { name: "notional", check: "decimal", align: Right }
            , Value { name: "premium_discount", check: "decimal", align: Right }
            , Value { name: "interest_rate", check: "decimal", align: Right }
            , Value { name: "purchase_date", check: "date", align: Right }
            , Value { name: "purchase_price", check: "decimal", align: Right }
            , Value { name: "initial_exchange_date", check: "date", align: Right }
            , Value { name: "termination_date", check: "date", align: Right }
            , Value { name: "termination_price", check: "decimal", align: Right }
            , Value { name: "periodic_payment_amount", check: "decimal", align: Right }
            , Value { name: "rate_reset_cycle", check: "cycle", align: Right }
            , Value { name: "interest_rate_cycle", check: "cycle", align: Right }
            , Value { name: "principal_redemption_cycle", check: "cycle", align: Right }
            , Value { name: "interest_rate_ctr", check: "assertionCtx", align: Right }
            , Value { name: "payoff_ctr", check: "assertion", align: Right }
            -- Any collateral-related code is commented out, until implemented properly
            -- , Value { name: "collateral", check: "integer", align: Right }
            ]
        , colour: blockColour (ActusContractType NegativeAmortizer)
        , previousStatement: Just (show BaseContractType)
        , inputsInline: Just false
        }
        defaultBlockDefinition

toDefinition (ActusValueType ActusDate) =
  BlockDefinition
    $ merge
        { type: show ActusDate
        , message0: "year %1 month %2 day %3"
        , args0:
            [ Number { name: "yyyy", value: 2020.0, min: Just 1900.0, max: Just 9999.0, precision: Just 0.0 }
            , Number { name: "mm", value: 10.0, min: Just 1.0, max: Just 12.0, precision: Just 0.0 }
            , Number { name: "dd", value: 1.0, min: Just 1.0, max: Just 31.0, precision: Just 0.0 }
            ]
        , colour: blockColour (ActusValueType ActusDate)
        , output: Just "date"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ActusValueType ActusCycleType) =
  BlockDefinition
    $ merge
        { type: show ActusCycleType
        , message0: "Cycle with anchor %1 and period %2 of %3 with short stub"
        , args0:
            [ Value { name: "anchor", check: "date", align: Right }
            , Input { name: "value", text: "1", spellcheck: false }
            , Value { name: "period", check: "period", align: Right }
            ]
        , colour: blockColour (ActusValueType ActusCycleType)
        , output: Just "cycle"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ActusValueType ActusDecimalType) =
  BlockDefinition
    $ merge
        { type: show ActusDecimalType
        , message0: "decimal %1"
        , args0:
            [ Input { name: "value", text: "1000", spellcheck: false }
            ]
        , colour: blockColour (ActusValueType ActusDecimalType)
        , inputsInline: Just false
        , output: Just "decimal"
        }
        defaultBlockDefinition

toDefinition (ActusValueType ActusIntegerType) =
  BlockDefinition
    $ merge
        { type: show ActusIntegerType
        , message0: "integer %1"
        , args0:
            [ Input { name: "value", text: "10000", spellcheck: false }
            ]
        , colour: blockColour (ActusValueType ActusIntegerType)
        , inputsInline: Just false
        , output: Just "integer"
        }
        defaultBlockDefinition

toDefinition (ActusValueType ActusAssertionContextType) =
  BlockDefinition
    $ merge
        { type: show ActusAssertionContextType
        , message0: "min interest rate %1, max interest rate %2"
        , args0:
            [ Input { name: "min_rrmo", text: "0", spellcheck: false }
            , Input { name: "max_rrmo", text: "1000", spellcheck: false }
            ]
        , colour: blockColour (ActusValueType ActusAssertionContextType)
        , inputsInline: Just false
        , output: Just "assertionCtx"
        }
        defaultBlockDefinition

toDefinition (ActusValueType ActusAssertionType) =
  BlockDefinition
    $ merge
        { type: show ActusAssertionType
        , message0: "net present value is more than %1 against zero-risk bond with rate %2"
        , args0:
            [ Input { name: "npv", text: "0", spellcheck: false }
            , Input { name: "rate", text: "0", spellcheck: false }
            ]
        , colour: blockColour (ActusValueType ActusAssertionType)
        , inputsInline: Just false
        , output: Just "assertion"
        }
        defaultBlockDefinition

toDefinition (ActusPeriodType PeriodDayType) =
  BlockDefinition
    $ merge
        { type: show PeriodDayType
        , message0: "Days"
        , args0:
            []
        , colour: blockColour (ActusPeriodType PeriodDayType)
        , inputsInline: Just true
        , output: Just "period"
        }
        defaultBlockDefinition

toDefinition (ActusPeriodType PeriodMonthType) =
  BlockDefinition
    $ merge
        { type: show PeriodMonthType
        , message0: "Months"
        , args0: []
        , colour: blockColour (ActusPeriodType PeriodMonthType)
        , inputsInline: Just true
        , output: Just "period"
        }
        defaultBlockDefinition

toDefinition (ActusPeriodType PeriodQuarterType) =
  BlockDefinition
    $ merge
        { type: show PeriodQuarterType
        , message0: "Quarters"
        , args0: []
        , colour: blockColour (ActusPeriodType PeriodQuarterType)
        , inputsInline: Just true
        , output: Just "period"
        }
        defaultBlockDefinition

toDefinition (ActusPeriodType PeriodYearType) =
  BlockDefinition
    $ merge
        { type: show PeriodYearType
        , message0: "Years"
        , args0: []
        , colour: blockColour (ActusPeriodType PeriodYearType)
        , output: Just "period"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toolbox :: Toolbox
toolbox =
  CategoryToolbox
    [ category "Contracts" actusColour $ map (leaf <<< show) actusContractTypes
    , category "Values" valueColour $ map (leaf <<< show) actusValueTypes
    , category "Periods" periodColour $ map (leaf <<< show) actusPeriodTypes
    ]

workspaceBlocks :: forall a b. HTML a b
workspaceBlocks =
  xml [ id_ "actusWorkspaceBlocks", style "display:none" ]
    [ block [ blockType (show BaseContractType), x "13", y "187", id_ rootBlockName ] []
    ]

parse :: forall a. Parser a -> String -> Either String a
parse p = lmap show <<< runParser' (parens p <|> p)

buildGenerator :: Blockly -> Effect Generator
buildGenerator blockly = do
  generator <- mkGenerator blockly "Actus"
  let
    mkGenFun :: forall a t. Show a => Show t => t -> (Block -> Either String a) -> Effect Unit
    mkGenFun blockType f = insertGeneratorFunction generator (show blockType) ((rmap show) <<< f)
  traverse_ (\t -> mkGenFun t (baseContractDefinition generator)) [ BaseContractType ]
  traverse_ (\t -> mkGenFun t (blockDefinition t generator)) actusContractTypes
  traverse_ (\t -> mkGenFun t (blockDefinition t generator)) actusValueTypes
  traverse_ (\t -> mkGenFun t (blockDefinition t generator)) actusPeriodTypes
  pure generator

class HasBlockDefinition a b | a -> b where
  blockDefinition :: a -> Generator -> Block -> Either String b

baseContractDefinition :: Generator -> Block -> Either String ActusContract
baseContractDefinition g block = do
  code <- statementToCode g block (show BaseContractType)
  json <- catch $ runExcept $ parseJSON code
  catch $ runExcept (decode json :: F ActusContract)

newtype ActusContract
  = ActusContract
  { contractType :: CT
  , startDate :: ActusValue
  , initialExchangeDate :: ActusValue
  , maturityDate :: ActusValue
  , terminationDate :: ActusValue
  , terminationPrice :: ActusValue
  , periodicPaymentAmount :: ActusValue
  , purchaseDate :: ActusValue
  , purchasePrice :: ActusValue
  , dayCountConvention :: ActusValue
  , endOfMonthConvention :: ActusValue
  , rateReset :: ActusValue
  , notional :: ActusValue
  , premiumDiscount :: ActusValue
  , interestRate :: ActusValue
  , interestRateCycle :: ActusValue
  , principalRedemptionCycle :: ActusValue
  , interestCalculationBaseCycle :: ActusValue
  , assertionCtx :: ActusValue
  , assertion :: ActusValue
  -- Any collateral-related code is commented out, until implemented properly
  -- , collateral :: ActusValue
  }

derive instance actusContract :: Generic ActusContract _

derive instance actusContractNewtype :: Newtype ActusContract _

instance showActusContract :: Show ActusContract where
  show = encodeJSON

instance encodeJsonActusContract :: Encode ActusContract where
  encode a = genericEncode aesonCompatibleOptions a

instance decodeJsonActusContract :: Decode ActusContract where
  decode a = genericDecode aesonCompatibleOptions a

data ActusValue
  = DateValue String String String
  | CycleValue ActusValue BigInteger ActusPeriodType
  | DecimalValue Number
  | IntegerValue BigInteger
  | ActusAssertionCtx Number Number
  | ActusAssertionNpv Number Number
  | NoActusValue
  | ActusError String

derive instance actusValue :: Generic ActusValue _

instance showActusValue :: Show ActusValue where
  show = encodeJSON

instance encodeJsonActusValue :: Encode ActusValue where
  encode a = genericEncode aesonCompatibleOptions a

instance decodeJsonActusValue :: Decode ActusValue where
  decode a = genericDecode aesonCompatibleOptions a

catch :: forall a b. Show a => Either a b -> Either String b
catch = lmap show

parseFieldActusValueJson :: Generator -> Block -> String -> ActusValue
parseFieldActusValueJson g block name = Either.either (const NoActusValue) identity result
  where
  result = do
    value <- statementToCode g block name
    parsed <- catch $ runExcept $ parseJSON value
    let
      decoded = decode parsed :: F ActusValue
    catch $ runExcept $ decoded

parseFieldActusPeriodJson :: Generator -> Block -> String -> Maybe ActusPeriodType
parseFieldActusPeriodJson g block name = Either.hush result
  where
  result = do
    value <- statementToCode g block name
    parsed <- catch $ runExcept $ parseJSON value
    let
      decoded = decode parsed :: F ActusPeriodType
    catch $ runExcept $ decoded

parseActusContractType :: Block -> CT
parseActusContractType b = case getType b of
  "PaymentAtMaturity" -> PAM
  "LinearAmortizer" -> LAM
  "NegativeAmortizer" -> NAM
  _ -> PAM

parseActusJsonCode :: String -> Either String ContractTerms
parseActusJsonCode str = do
  parsed <- catch $ runExcept $ parseJSON str
  let
    decoded = decode parsed :: F ActusContract
  result <- catch $ runExcept $ decoded
  actusContractToTerms result

instance hasBlockDefinitionActusContract :: HasBlockDefinition ActusContractType ActusContract where
  blockDefinition _ g block = case parseActusContractType block of
    PAM ->
      Either.Right
        $ ActusContract
            { contractType: parseActusContractType block
            , startDate: parseFieldActusValueJson g block "start_date"
            , initialExchangeDate: parseFieldActusValueJson g block "initial_exchange_date"
            , maturityDate: parseFieldActusValueJson g block "maturity_date"
            , terminationDate: parseFieldActusValueJson g block "termination_date"
            , terminationPrice: parseFieldActusValueJson g block "termination_price"
            , periodicPaymentAmount: NoActusValue
            , purchaseDate: parseFieldActusValueJson g block "purchase_date"
            , purchasePrice: parseFieldActusValueJson g block "purchase_price"
            , dayCountConvention: parseFieldActusValueJson g block "day_count_convention"
            , endOfMonthConvention: parseFieldActusValueJson g block "end_of_month_convention"
            , rateReset: parseFieldActusValueJson g block "rate_reset_cycle"
            , notional: parseFieldActusValueJson g block "notional"
            , premiumDiscount: parseFieldActusValueJson g block "premium_discount"
            , interestRate: parseFieldActusValueJson g block "interest_rate"
            , interestRateCycle: parseFieldActusValueJson g block "interest_rate_cycle"
            , principalRedemptionCycle: NoActusValue
            , interestCalculationBaseCycle: NoActusValue
            , assertionCtx: parseFieldActusValueJson g block "interest_rate_ctr"
            , assertion: parseFieldActusValueJson g block "payoff_ctr"
            -- Any collateral-related code is commented out, until implemented properly
            -- , collateral: parseFieldActusValueJson g block "collateral"
            }
    LAM ->
      Either.Right
        $ ActusContract
            { contractType: parseActusContractType block
            , startDate: parseFieldActusValueJson g block "start_date"
            , initialExchangeDate: parseFieldActusValueJson g block "initial_exchange_date"
            , maturityDate: parseFieldActusValueJson g block "maturity_date"
            , terminationDate: parseFieldActusValueJson g block "termination_date"
            , terminationPrice: parseFieldActusValueJson g block "termination_price"
            , periodicPaymentAmount: parseFieldActusValueJson g block "periodic_payment_amount"
            , purchaseDate: parseFieldActusValueJson g block "purchase_date"
            , purchasePrice: parseFieldActusValueJson g block "purchase_price"
            , dayCountConvention: parseFieldActusValueJson g block "day_count_convention"
            , endOfMonthConvention: parseFieldActusValueJson g block "end_of_month_convention"
            , rateReset: parseFieldActusValueJson g block "rate_reset_cycle"
            , notional: parseFieldActusValueJson g block "notional"
            , premiumDiscount: parseFieldActusValueJson g block "premium_discount"
            , interestRate: parseFieldActusValueJson g block "interest_rate"
            , interestRateCycle: parseFieldActusValueJson g block "interest_rate_cycle"
            , principalRedemptionCycle: parseFieldActusValueJson g block "principal_redemption_cycle"
            , interestCalculationBaseCycle: parseFieldActusValueJson g block "interest_calculation_base_cycle"
            , assertionCtx: parseFieldActusValueJson g block "interest_rate_ctr"
            , assertion: parseFieldActusValueJson g block "payoff_ctr"
            -- Any collateral-related code is commented out, until implemented properly
            -- , collateral: parseFieldActusValueJson g block "collateral"
            }
    NAM ->
      Either.Right
        $ ActusContract
            { contractType: parseActusContractType block
            , startDate: parseFieldActusValueJson g block "start_date"
            , initialExchangeDate: parseFieldActusValueJson g block "initial_exchange_date"
            , maturityDate: parseFieldActusValueJson g block "maturity_date"
            , terminationDate: parseFieldActusValueJson g block "termination_date"
            , terminationPrice: parseFieldActusValueJson g block "termination_price"
            , periodicPaymentAmount: parseFieldActusValueJson g block "periodic_payment_amount"
            , purchaseDate: parseFieldActusValueJson g block "purchase_date"
            , purchasePrice: parseFieldActusValueJson g block "purchase_price"
            , dayCountConvention: parseFieldActusValueJson g block "day_count_convention"
            , endOfMonthConvention: parseFieldActusValueJson g block "end_of_month_convention"
            , rateReset: parseFieldActusValueJson g block "rate_reset_cycle"
            , notional: parseFieldActusValueJson g block "notional"
            , premiumDiscount: parseFieldActusValueJson g block "premium_discount"
            , interestRate: parseFieldActusValueJson g block "interest_rate"
            , interestRateCycle: parseFieldActusValueJson g block "interest_rate_cycle"
            , principalRedemptionCycle: parseFieldActusValueJson g block "principal_redemption_cycle"
            , interestCalculationBaseCycle: parseFieldActusValueJson g block "interest_calculation_base_cycle"
            , assertionCtx: parseFieldActusValueJson g block "interest_rate_ctr"
            , assertion: parseFieldActusValueJson g block "payoff_ctr"
            -- Any collateral-related code is commented out, until implemented properly
            -- , collateral: parseFieldActusValueJson g block "collateral"
            }

instance hasBlockDefinitionValue :: HasBlockDefinition ActusValueType ActusValue where
  blockDefinition ActusDate g block = do
    yyyy <- getFieldValue block "yyyy"
    m <- getFieldValue block "mm"
    d <- getFieldValue block "dd"
    year <- Either.note ("can't parse integer: " <> yyyy) $ fromString yyyy
    month <- Either.note ("can't parse integer: " <> m) $ fromString m
    day <- Either.note ("can't parse integer: " <> d) $ fromString d
    safeYear <- Either.note "wrong year" $ toEnum year
    safeMonth <- Either.note "wrong month" $ toEnum month
    safeDay <- Either.note "wrong day" $ toEnum day
    let
      mm = if month < 10 then "0" <> m else m
    let
      dd = if day < 10 then "0" <> d else d
    let
      date = exactDate safeYear safeMonth safeDay
    pure
      $ if isJust date then
          DateValue yyyy mm dd
        else
          ActusError $ "Incorrect date: " <> yyyy <> "-" <> mm <> "-" <> dd
  blockDefinition ActusCycleType g block = do
    valueString <- getFieldValue block "value"
    value <- fromMaybe (Either.Left "can't parse bigint") $ Either.Right <$> BigInteger.fromString valueString
    let
      anchor = parseFieldActusValueJson g block "anchor"
    let
      period = parseFieldActusPeriodJson g block "period"
    pure $ fromMaybe NoActusValue $ CycleValue anchor value <$> period --todo validation: return value if date is invalid
  blockDefinition ActusDecimalType g block = do
    valueString <- getFieldValue block "value"
    value <- fromMaybe (Either.Left "can't parse numeric") $ Either.Right <$> parseFloat valueString
    pure $ DecimalValue value
  blockDefinition ActusIntegerType g block = do
    valueString <- getFieldValue block "value"
    value <- fromMaybe (Either.Left "can't parse numeric") $ Either.Right <$> BigInteger.fromString valueString
    pure $ IntegerValue value
  blockDefinition ActusAssertionContextType g block = do
    minValueString <- getFieldValue block "min_rrmo"
    minValue <- fromMaybe (Either.Left "can't parse numeric") $ Either.Right <$> parseFloat minValueString
    maxValueString <- getFieldValue block "max_rrmo"
    maxValue <- fromMaybe (Either.Left "can't parse numeric") $ Either.Right <$> parseFloat maxValueString
    pure $ ActusAssertionCtx minValue maxValue
  blockDefinition ActusAssertionType g block = do
    npvString <- getFieldValue block "npv"
    npv <- fromMaybe (Either.Left "can't parse numeric") $ Either.Right <$> parseFloat npvString
    zeroInterestString <- getFieldValue block "rate"
    zeroInterest <- fromMaybe (Either.Left "can't parse numeric") $ Either.Right <$> parseFloat zeroInterestString
    pure $ ActusAssertionNpv npv zeroInterest

instance hasBlockDefinitionPeriod :: HasBlockDefinition ActusPeriodType ActusPeriodType where
  blockDefinition x _ _ = pure x

actusDateToDay :: ActusValue -> Either String (Maybe String)
actusDateToDay (DateValue yyyy mm dd) = Either.Right $ Just $ yyyy <> "-" <> mm <> "-" <> dd --should be validated in a parser

actusDateToDay (ActusError msg) = Either.Left msg

actusDateToDay NoActusValue = Either.Right Nothing

actusDateToDay x = Either.Left $ "Unexpected: " <> show x -- should be unreachable

actusDecimalToNumber :: ActusValue -> Either String (Maybe Number)
actusDecimalToNumber (DecimalValue n) = Either.Right $ Just $ n

actusDecimalToNumber (ActusError msg) = Either.Left msg

actusDecimalToNumber NoActusValue = Either.Right Nothing

actusDecimalToNumber x = Either.Left $ "Unexpected: " <> show x

actusIntegerToNumber :: ActusValue -> Either String (Maybe BigInteger)
actusIntegerToNumber (IntegerValue n) = Either.Right $ Just $ n

actusIntegerToNumber (ActusError msg) = Either.Left msg

actusIntegerToNumber NoActusValue = Either.Right Nothing

actusIntegerToNumber x = Either.Left $ "Unexpected: " <> show x

blocklyCycleToCycle :: ActusValue -> Either String (Maybe Cycle)
blocklyCycleToCycle (CycleValue _ value period) =
  Either.Right $ Just
    $ Cycle
        { n: value
        , p:
            case period of
              PeriodYearType -> P_Y
              PeriodDayType -> P_D
              PeriodMonthType -> P_M
              PeriodQuarterType -> P_Q
        , stub: ShortStub
        }

blocklyCycleToCycle (ActusError msg) = Either.Left msg

blocklyCycleToCycle NoActusValue = Either.Right Nothing

blocklyCycleToCycle x = Either.Left $ "Unexpected: " <> show x -- should be unreachable

blocklyAssertionCtxToAssertionCtx :: ActusValue -> Either String (Maybe AssertionContext)
blocklyAssertionCtxToAssertionCtx (ActusAssertionCtx min max) =
  Either.Right $ Just
    $ AssertionContext
        { rrmoMin: min
        , rrmoMax: max
        }

blocklyAssertionCtxToAssertionCtx (ActusError msg) = Either.Left msg

blocklyAssertionCtxToAssertionCtx NoActusValue = Either.Right Nothing

blocklyAssertionCtxToAssertionCtx x = Either.Left $ "Unexpected: " <> show x -- should be unreachable

blocklyAssertionToAssertion :: ActusValue -> Either String (Maybe Assertion)
blocklyAssertionToAssertion (ActusAssertionNpv npv rate) =
  Either.Right $ Just
    $ NpvAssertionAgainstZeroRiskBond
        { zeroRiskInterest: rate
        , expectedNpv: npv
        }

blocklyAssertionToAssertion (ActusError msg) = Either.Left msg

blocklyAssertionToAssertion NoActusValue = Either.Right Nothing

blocklyAssertionToAssertion x = Either.Left $ "Unexpected: " <> show x -- should be unreachable

blocklyCycleToAnchor :: ActusValue -> Either String (Maybe ActusValue)
blocklyCycleToAnchor (CycleValue anchor _ _) = Either.Right $ Just anchor

blocklyCycleToAnchor (ActusError msg) = Either.Left msg

blocklyCycleToAnchor NoActusValue = Either.Right Nothing

blocklyCycleToAnchor x = Either.Left $ "Unexpected: " <> show x -- should be unreachable

actusContractToTerms :: ActusContract -> Either String ContractTerms
actusContractToTerms raw = do
  let
    c = (unwrap raw)
  contractType <- Either.Right c.contractType
  startDate <- Either.note "start date is a mandatory field!" <$> actusDateToDay c.startDate >>= identity
  maturityDate <- actusDateToDay c.maturityDate
  initialExchangeDate <- fromMaybe startDate <$> actusDateToDay c.initialExchangeDate
  terminationDate <- actusDateToDay c.terminationDate
  terminationPrice <- actusDecimalToNumber c.terminationPrice
  periodicPaymentAmount <- actusDecimalToNumber c.periodicPaymentAmount
  purchaseDate <- actusDateToDay c.purchaseDate
  purchasePrice <- actusDecimalToNumber c.purchasePrice
  rateResetCycle <- blocklyCycleToCycle c.rateReset
  rateResetAnchorValue <- blocklyCycleToAnchor c.rateReset
  rateResetAnchor <- sequence $ actusDateToDay <$> rateResetAnchorValue
  notional <- actusDecimalToNumber c.notional
  premium <- fromMaybe 0.0 <$> actusDecimalToNumber c.premiumDiscount
  interestRateCycle <- blocklyCycleToCycle c.interestRateCycle
  interestRateAnchorValue <- blocklyCycleToAnchor c.interestRateCycle
  principalRedemptionCycle <- blocklyCycleToCycle c.principalRedemptionCycle
  principalRedemptionAnchorValue <- blocklyCycleToAnchor c.principalRedemptionCycle
  principalRedemptionAnchor <- sequence $ actusDateToDay <$> principalRedemptionAnchorValue
  interestCalculationBaseCycle <- blocklyCycleToCycle c.interestCalculationBaseCycle
  interestCalculationBaseAnchorValue <- blocklyCycleToAnchor c.interestCalculationBaseCycle
  interestCalculationBaseAnchor <- sequence $ actusDateToDay <$> interestCalculationBaseAnchorValue
  interestRateAnchor <- sequence $ actusDateToDay <$> interestRateAnchorValue
  interestRateUnchecked <- actusDecimalToNumber c.interestRate
  interestRate <-
    if isJust interestRateCycle && isNothing interestRateUnchecked then
      Either.Left "Please specify interest rate"
    else
      Either.Right interestRateUnchecked
  assertionCtx <- blocklyAssertionCtxToAssertionCtx c.assertionCtx
  assertion <- blocklyAssertionToAssertion c.assertion
  let
    constraint ctx =
      Assertions
        { context: ctx
        , assertions:
            ( case assertion of
                Just x -> [ x ]
                Nothing -> []
            )
        }
  -- Any collateral-related code is commented out, until implemented properly
  -- collateral <- actusIntegerToNumber c.collateral
  pure
    $ ContractTerms
        { contractId: "0"
        , contractType: contractType
        , ct_IED: Just initialExchangeDate
        , ct_SD: startDate
        , ct_MD: maturityDate
        , ct_TD: terminationDate
        , ct_PRNXT: periodicPaymentAmount
        , ct_PRD: purchaseDate
        , ct_CNTRL: CR_ST
        , ct_PDIED: Just premium
        , ct_NT: notional
        , ct_PPRD: purchasePrice
        , ct_PTD: terminationPrice
        , ct_DCC: Just DCC_A_360
        , ct_PPEF: Just PPEF_N
        , ct_PRF: Just PRF_PF
        , scfg:
            ScheduleConfig
              { calendar: []
              , eomc: Just EOMC_EOM
              , bdc: Just BDC_NULL
              }
        , ct_PYRT: Just 0.0
        , ct_PYTP: Just PYTP_A
        , ct_cPYRT: 0.0
        , ct_OPCL: Nothing
        , ct_OPANX: Nothing
        , ct_SCIED: Just 0.0
        , ct_SCEF: Just SE_000
        , ct_SCCL: Nothing
        , ct_SCANX: Nothing
        , ct_SCCDD: Just 0.0
        , ct_RRCL: rateResetCycle
        , ct_RRANX: rateResetAnchor >>= identity
        , ct_RRNXT: Nothing
        , ct_RRSP: Just 0.0
        , ct_RRMLT: Just 1.0
        , ct_RRPF: Just $ -1.0
        , ct_RRPC: Just 1.0
        , ct_RRLC: Just 1.0
        , ct_RRLF: Just 0.0
        , ct_IPCED: Nothing
        , ct_IPCL: interestRateCycle
        , ct_IPANX: interestRateAnchor >>= identity
        , ct_IPNR: interestRate
        , ct_IPAC: Nothing
        , ct_PRCL: principalRedemptionCycle
        , ct_PRANX: principalRedemptionAnchor >>= identity
        , ct_IPCB: Just IPCB_NT -- Default for now
        , ct_IPCBA: Just 0.0 -- Default for now
        , ct_IPCBCL: interestCalculationBaseCycle -- unused due to above defaults for now
        , ct_IPCBANX: interestCalculationBaseAnchor >>= identity -- also unused
        , ct_FECL: Nothing
        , ct_FEANX: Nothing
        , ct_FEAC: Nothing
        , ct_FEB: Just FEB_N
        , ct_FER: Just 0.0
        , ct_CURS: false
        , constraints: constraint <$> assertionCtx
        -- Any collateral-related code is commented out, until implemented properly
        -- , collateralAmount: fromMaybe (BigInteger.fromInt 0) collateral
        , collateralAmount: BigInteger.fromInt 0
        }

aesonCompatibleOptions :: Options
aesonCompatibleOptions =
  defaultOptions
    { unwrapSingleConstructors = true
    , sumEncoding = aesonSumEncoding
    }
