{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Main(main) where

import           Control.Lens
import           Control.Monad               (forM_, guard, replicateM, void)
import           Control.Monad.Trans.Except  (runExcept)
import qualified Data.Aeson                  as JSON
import qualified Data.Aeson.Extras           as JSON
import qualified Data.Aeson.Internal         as Aeson
import qualified Data.ByteString             as BSS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Default                (Default (def))
import           Data.Either                 (isLeft, isRight)
import           Data.Foldable               (fold, foldl', traverse_)
import           Data.List                   (sort)
import qualified Data.Map                    as Map
import           Data.Monoid                 (Sum (..))
import qualified Data.Set                    as Set
import           Data.String                 (IsString (fromString))
import           Hedgehog                    (Property, forAll, property)
import qualified Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Ledger
import qualified Ledger.Ada                  as Ada
import           Ledger.Bytes                as Bytes
import qualified Ledger.Constraints.OffChain as OC
import qualified Ledger.Contexts             as Validation
import qualified Ledger.Crypto               as Crypto
import           Ledger.Fee                  (FeeConfig (..), calcFees)
import qualified Ledger.Generators           as Gen
import qualified Ledger.Index                as Index
import qualified Ledger.Interval             as Interval
import qualified Ledger.Scripts              as Scripts
import           Ledger.TimeSlot             (SlotConfig (..))
import qualified Ledger.TimeSlot             as TimeSlot
import           Ledger.Value                (CurrencySymbol, Value (Value))
import qualified Ledger.Value                as Value
import qualified PlutusCore.Default          as PLC
import           PlutusTx                    (CompiledCode, applyCode, liftCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as AMap
import qualified PlutusTx.AssocMap           as AssocMap
import qualified PlutusTx.Builtins           as Builtins
import qualified PlutusTx.Prelude            as PlutusTx
import           Test.Tasty                  hiding (after)
import           Test.Tasty.HUnit            (testCase)
import qualified Test.Tasty.HUnit            as HUnit
import           Test.Tasty.Hedgehog         (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests" [
    testGroup "UTXO model" [
        testProperty "initial transaction is valid" initialTxnValid
        ],
    testGroup "intervals" [
        testProperty "member" intvlMember,
        testProperty "contains" intvlContains
        ],
    testGroup "values" [
        testProperty "additive identity" valueAddIdentity,
        testProperty "additive inverse" valueAddInverse,
        testProperty "scalar identity" valueScalarIdentity,
        testProperty "scalar distributivity" valueScalarDistrib
        ],
    testGroup "Etc." [
        testProperty "splitVal" splitVal,
        testProperty "encodeByteString" encodeByteStringTest,
        testProperty "encodeSerialise" encodeSerialiseTest
        ],
    testGroup "LedgerBytes" [
        testProperty "show-fromHex" ledgerBytesShowFromHexProp,
        testProperty "toJSON-fromJSON" ledgerBytesToJSONProp
        ],
    testGroup "Value" ([
        testProperty "Value ToJSON/FromJSON" (jsonRoundTrip Gen.genValue),
        testProperty "CurrencySymbol ToJSON/FromJSON" (jsonRoundTrip $ Value.currencySymbol <$> Gen.genSizedByteStringExact 32),
        testProperty "TokenName ToJSON/FromJSON" (jsonRoundTrip Gen.genTokenName),
        testProperty "TokenName looks like escaped bytestring ToJSON/FromJSON" (jsonRoundTrip . pure $ ("\NUL0xc0ffee" :: Value.TokenName)),
        testProperty "CurrencySymbol IsString/Show" currencySymbolIsStringShow
        ] ++ (let   vlJson :: BSL.ByteString
                    vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"ab01ff\"},[[{\"unTokenName\":\"myToken\"},50]]]]}"
                    vlValue = Value.singleton "ab01ff" "myToken" 50
                in byteStringJson vlJson vlValue)
          ++ (let   vlJson :: BSL.ByteString
                    vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},50]]]]}"
                    vlValue = Ada.lovelaceValueOf 50
                in byteStringJson vlJson vlValue)),
    testGroup "Constraints" [
        testProperty "missing value spent" missingValueSpentProp
        ],
    testGroup "Fee" [
        testProperty "calcFees" calcFeesTest
        ],
    testGroup "TimeSlot" [
        testProperty "time range of starting slot" initialSlotToTimeProp,
        testProperty "slot of starting time range" initialTimeToSlotProp,
        testProperty "slot number >=0 when converting from time" slotIsPositiveProp,
        testProperty "slotRange to timeRange inverse property" slotToTimeInverseProp,
        testProperty "timeRange to slotRange inverse property" timeToSlotInverseProp,
        testProperty "conversion inverse property" slotToTimeInverseProp,
        testProperty "slot to time range inverse to slot range"
          slotToTimeRangeBoundsInverseProp,
        testProperty "slot to time range has lower bound <= upper bound"
          slotToTimeRangeHasLowerAndUpperBoundsProp
        ]
    ]

initialTxnValid :: Property
initialTxnValid = property $ do
    (i, _) <- forAll . pure $ Gen.genInitialTransaction Gen.generatorModel
    Gen.assertValid i Gen.emptyChain

splitVal :: Property
splitVal = property $ do
    i <- forAll $ Gen.integral $ Range.linear 1 (100000 :: Integer)
    n <- forAll $ Gen.integral $ Range.linear 1 100
    vs <- forAll $ Gen.splitVal n i
    Hedgehog.assert $ sum vs == i
    Hedgehog.assert $ length vs <= n

valueAddIdentity :: Property
valueAddIdentity = property $ do
    vl1 <- forAll Gen.genValue
    Hedgehog.assert $ vl1 == (vl1 PlutusTx.+ PlutusTx.zero)
    Hedgehog.assert $ vl1 == (PlutusTx.zero PlutusTx.+ vl1)

valueAddInverse :: Property
valueAddInverse = property $ do
    vl1 <- forAll Gen.genValue
    let vl1' = PlutusTx.negate vl1
    Hedgehog.assert $ PlutusTx.zero == (vl1 PlutusTx.+ vl1')

valueScalarIdentity :: Property
valueScalarIdentity = property $ do
    vl1 <- forAll Gen.genValue
    Hedgehog.assert $ vl1 == PlutusTx.scale 1 vl1

valueScalarDistrib :: Property
valueScalarDistrib = property $ do
    vl1 <- forAll Gen.genValue
    vl2 <- forAll Gen.genValue
    scalar <- forAll (Gen.integral (fromIntegral <$> Range.linearBounded @Int))
    let r1 = PlutusTx.scale scalar (vl1 PlutusTx.+ vl2)
        r2 = PlutusTx.scale scalar vl1 PlutusTx.+ PlutusTx.scale scalar vl2
    Hedgehog.assert $ r1 == r2

intvlMember :: Property
intvlMember = property $ do
    (i1, i2) <- forAll $ (,) <$> Gen.integral (fromIntegral <$> Range.linearBounded @Int) <*> Gen.integral (fromIntegral <$> Range.linearBounded @Int)
    let (from, to) = (min i1 i2, max i1 i2)
        i          = Interval.interval (Slot from) (Slot to)
    Hedgehog.assert $ Interval.member (Slot from) i || Interval.isEmpty i
    Hedgehog.assert $ not (Interval.member (Slot (from-1)) i) || Interval.isEmpty i
    Hedgehog.assert $ Interval.member (Slot to) i || Interval.isEmpty i
    Hedgehog.assert $ not (Interval.member (Slot (to+1)) i) || Interval.isEmpty i

intvlContains :: Property
intvlContains = property $ do
    -- generate two intervals from a sorted list of ints
    -- the outer interval contains the inner interval
    ints <- forAll $ traverse (const $ Gen.integral (fromIntegral <$> Range.linearBounded @Int)) [1..4]
    let [i1, i2, i3, i4] = Slot <$> sort ints
        outer = Interval.interval i1 i4
        inner = Interval.interval i2 i3

    Hedgehog.assert $ Interval.contains outer inner

encodeByteStringTest :: Property
encodeByteStringTest = property $ do
    bs <- forAll $ Gen.bytes $ Range.linear 0 1000
    let enc    = JSON.String $ JSON.encodeByteString bs
        result = Aeson.iparse JSON.decodeByteString enc

    Hedgehog.assert $ result == Aeson.ISuccess bs

encodeSerialiseTest :: Property
encodeSerialiseTest = property $ do
    txt <- forAll $ Gen.text (Range.linear 0 1000) Gen.unicode
    let enc    = JSON.String $ JSON.encodeSerialise txt
        result = Aeson.iparse JSON.decodeSerialise enc

    Hedgehog.assert $ result == Aeson.ISuccess txt

jsonRoundTrip :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => Hedgehog.Gen a -> Property
jsonRoundTrip gen = property $ do
    bts <- forAll gen
    let enc    = JSON.toJSON bts
        result = Aeson.iparse JSON.parseJSON enc

    Hedgehog.assert $ result == Aeson.ISuccess bts

ledgerBytesShowFromHexProp :: Property
ledgerBytesShowFromHexProp = property $ do
    bts <- forAll $ LedgerBytes <$> Gen.genSizedByteString 32
    let result = Bytes.fromHex $ fromString $ show bts

    Hedgehog.assert $ result == Right bts

ledgerBytesToJSONProp :: Property
ledgerBytesToJSONProp = property $ do
    bts <- forAll $ LedgerBytes <$> Gen.genSizedByteString 32
    let enc    = JSON.toJSON bts
        result = Aeson.iparse JSON.parseJSON enc

    Hedgehog.assert $ result == Aeson.ISuccess bts

currencySymbolIsStringShow :: Property
currencySymbolIsStringShow = property $ do
    cs <- forAll $ Value.currencySymbol <$> Gen.genSizedByteStringExact 32
    let cs' = fromString (show cs)
    Hedgehog.assert $ cs' == cs

-- byteStringJson :: (Eq a, JSON.FromJSON a) => BSL.ByteString -> a -> [TestCase]
byteStringJson jsonString value =
    [ testCase "decoding" $
        HUnit.assertEqual "Simple Decode" (Right value) (JSON.eitherDecode jsonString)
    , testCase "encoding" $ HUnit.assertEqual "Simple Encode" jsonString (JSON.encode value)
    ]

-- | Check that 'missingValueSpent' is the smallest value needed to
--   meet the requirements.
missingValueSpentProp :: Property
missingValueSpentProp = property $ do
    let valueSpentBalances = Gen.choice
            [ OC.provided <$> nonNegativeValue
            , OC.required <$> nonNegativeValue
            ]
        empty = OC.ValueSpentBalances mempty mempty
    balances <- foldl (<>) empty <$> forAll (Gen.list (Range.linear 0 10000) valueSpentBalances)
    let missing = OC.missingValueSpent balances
        actual = OC.vbsProvided balances
    Hedgehog.annotateShow missing
    Hedgehog.annotateShow actual
    Hedgehog.assert (OC.vbsRequired balances `Value.leq` (actual <> missing))

    -- To make sure that this is indeed the smallest value meeting
    -- the requirements, we reduce it by one and check that the property doesn't
    -- hold anymore.
    smaller <- forAll (reduceByOne missing)
    forM_ smaller $ \smaller' ->
        Hedgehog.assert (not (OC.vbsRequired balances `Value.leq` (actual <> smaller')))

-- | Reduce one of the elements in a 'Value' by one.
--   Returns 'Nothing' if the value contains no positive
--   elements.
reduceByOne :: Hedgehog.MonadGen m => Value -> m (Maybe Value)
reduceByOne (Value.Value value) = do
    let flat = do
            (currency, rest) <- AMap.toList value
            (tokenName, amount) <- AMap.toList rest
            guard (amount > 0)
            pure (currency, tokenName, pred amount)
    if null flat
        then pure Nothing
        else (\(cur, tok, amt) -> Just $ Value.singleton cur tok amt) <$> Gen.element flat

-- | A 'Value' with non-negative entries taken from a relatively
--   small pool of MPS hashes and token names.
nonNegativeValue :: Hedgehog.MonadGen m => m Value
nonNegativeValue =
    let mpsHashes = ["ffff", "dddd", "cccc", "eeee", "1010"]
        tokenNames = ["a", "b", "c", "d"]
    in Value.singleton
        <$> Gen.element mpsHashes
        <*> Gen.element tokenNames
        <*> Gen.integral (Range.linear 0 10000)

calcFeesTest :: Property
calcFeesTest = property $ do
  let feeCfg = FeeConfig 10 0.3
  Hedgehog.assert $ calcFees feeCfg 11 == Ada.lovelaceOf 13

-- | Asserting that time range of 'scSlotZeroTime' to 'scSlotZeroTime + scSlotLength'
-- is 'Slot 0' and the time after that is 'Slot 1'.
initialSlotToTimeProp :: Property
initialSlotToTimeProp = property $ do
  sc <- forAll slotConfigGen
  n <- forAll $ Gen.int (fromInteger <$> Range.linear 0 (fromIntegral $ scSlotLength sc))
  let diff = DiffMilliSeconds $ toInteger n
  let time = TimeSlot.scSlotZeroTime sc + fromMilliSeconds diff
  if diff >= fromIntegral (scSlotLength sc)
     then Hedgehog.assert $ TimeSlot.posixTimeToEnclosingSlot sc time == Slot 1
     else Hedgehog.assert $ TimeSlot.posixTimeToEnclosingSlot sc time == Slot 0

-- | Property that the interval time of 'Slot 0' goes from 'scSlotZeroTime' to
-- 'scSlotZeroTime + scSlotLength - 1'
initialTimeToSlotProp :: Property
initialTimeToSlotProp = property $ do
  sc <- forAll slotConfigGen
  let beginTime = TimeSlot.scSlotZeroTime sc
      endTime = TimeSlot.scSlotZeroTime sc + fromIntegral (TimeSlot.scSlotLength sc) - 1
      expectedTimeRange = interval beginTime endTime
  Hedgehog.assert $ TimeSlot.slotToPOSIXTimeRange sc 0 == expectedTimeRange

-- | Converting from POSIXTime to Slot should always produce a non negative
-- slot number.
slotIsPositiveProp :: Property
slotIsPositiveProp = property $ do
  sc <- forAll slotConfigGen
  posixTime <- forAll $ posixTimeGen sc
  Hedgehog.assert $ TimeSlot.posixTimeToEnclosingSlot sc posixTime >= 0

-- | Inverse property between 'slotRangeToPOSIXTimeRange' and
-- 'posixTimeRangeToSlotRange' from a 'SlotRange'.
slotToTimeInverseProp :: Property
slotToTimeInverseProp = property $ do
  sc <- forAll slotConfigGen
  slotRange <- forAll slotRangeGen
  Hedgehog.assert $
    slotRange == TimeSlot.posixTimeRangeToSlotRange sc
      (TimeSlot.slotRangeToPOSIXTimeRange sc slotRange)

-- | Inverse property between 'posixTimeRangeToSlotRange' and
-- 'slotRangeToPOSIXTimeRange' from a 'POSIXTimeRange'.
timeToSlotInverseProp :: Property
timeToSlotInverseProp = property $ do
  sc <- forAll slotConfigGen
  timeRange <- forAll $ timeRangeGen sc
  Hedgehog.assert $
    Interval.contains
      (TimeSlot.slotRangeToPOSIXTimeRange sc (TimeSlot.posixTimeRangeToSlotRange sc timeRange))
      timeRange

-- | 'POSIXTimeRange' from 'Slot' should have lower bound lower or equal than upper bound
slotToTimeRangeHasLowerAndUpperBoundsProp :: Property
slotToTimeRangeHasLowerAndUpperBoundsProp = property $ do
  sc <- forAll slotConfigGen
  slot <- forAll slotGen
  let (Interval (LowerBound t1 _) (UpperBound t2 _)) = TimeSlot.slotToPOSIXTimeRange sc slot
  Hedgehog.assert $ t1 <= t2

-- | Inverse property between 'slotToPOSIXTimeRange and 'posixTimeSlot'.
--
-- Given a slot 's', and the resulting time range [a,b] from
-- 'slotToPOSIXTimeRange s', verify that 'posixTimeSlot a == s' and
-- 'posixTimeSlot b == s'.
slotToTimeRangeBoundsInverseProp :: Property
slotToTimeRangeBoundsInverseProp = property $ do
  sc <- forAll slotConfigGen
  slot <- forAll slotGen
  let slotRange = PlutusTx.fmap (TimeSlot.posixTimeToEnclosingSlot sc)
                                $ TimeSlot.slotToPOSIXTimeRange sc slot
  Hedgehog.assert $ interval slot slot == slotRange

-- | Generate an 'Interval where the lower bound if less or equal than the
-- upper bound.
intervalGen :: (MonadFail m, Hedgehog.MonadGen m, Ord a)
            => m a
            -> m (Interval a)
intervalGen gen = do
  [b, e] <- sort <$> replicateM 2 gen
  return $ Interval.interval b e

-- | Generate a 'SlotRange' where the lower bound if less or equal than the
-- upper bound.
slotRangeGen :: (MonadFail m, Hedgehog.MonadGen m) => m SlotRange
slotRangeGen = intervalGen slotGen

-- | Generate a 'POSIXTimeRange' where the lower bound if less or equal than the
-- upper bound.
timeRangeGen :: (MonadFail m, Hedgehog.MonadGen m) => SlotConfig -> m POSIXTimeRange
timeRangeGen sc = intervalGen $ posixTimeGen sc

-- | Generate a 'Slot' where the lowest slot number is 0.
slotGen :: (Hedgehog.MonadGen m) => m Slot
slotGen = Slot <$> Gen.integral (fromIntegral <$> Range.linear 0 10000)

-- | Generate a 'POSIXTime' where the lowest value is 'scSlotZeroTime' given a
-- 'SlotConfig'.
posixTimeGen :: (Hedgehog.MonadGen m) => SlotConfig -> m POSIXTime
posixTimeGen sc = do
  let beginTime = getPOSIXTime $ TimeSlot.scSlotZeroTime sc
  POSIXTime <$> Gen.integral (Range.linear beginTime (beginTime + 10000000))

-- | Generate a 'SlotConfig' where the slot length goes from 1 to 100000
-- ms and the time of Slot 0 is the default 'scSlotZeroTime'.
slotConfigGen :: Hedgehog.MonadGen m => m SlotConfig
slotConfigGen = do
  sl <- Gen.integral (Range.linear 1 1000000)
  return $ def { TimeSlot.scSlotLength = sl }
