{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Main(main) where

import qualified Codec.CBOR.FlatTerm as FlatTerm
import           Codec.Serialise     (deserialiseOrFail, serialise)
import qualified Codec.Serialise     as Serialise
import           Hedgehog            (MonadGen, Property, PropertyT, annotateShow, assert, forAll, property, tripping)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           PlutusCore.Data     (Data (..))
import           PlutusTx.Builtins   (invertInteger, powModInteger, probablyPrimeInteger)
import           PlutusTx.Numeric    (negate)
import           PlutusTx.Ratio      (Rational, denominator, numerator, recip, (%))
import           PlutusTx.Sqrt       (Sqrt (..), isqrt, rsqrt)
import           Prelude             hiding (Rational, negate, recip)
import           Test.Tasty
import           Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "plutus-tx"
  [
    powModIntegerTests
  , invertIntegerTests
  , probablyPrimeTests
  , serdeTests
  , sqrtTests
  , ratioTests
  ]

sqrtTests :: TestTree
sqrtTests = testGroup "isqrt/rsqrt tests"
  [ testProperty "isqrt x^2 = x" isqrtRoundTrip
  , testProperty "rsqrt (a/b)^2 = integer part of a/b" rsqrtRoundTrip
  , testProperty "rsqrt (-x/b) = Imaginary" rsqrtRoundTripImaginary
  ]

rsqrtRoundTripImaginary :: Property
rsqrtRoundTripImaginary = property $ do
  let numerators   = Gen.integral (Range.linear (-100000) 0)
  let denominators = Gen.integral (Range.linear 1 100000)

  -- Note: We're using the fact that (a % -b) is reduced to (-a % b)
  -- so we only need to test those negative numbers.

  a <- forAll numerators
  b <- forAll denominators

  let x      = a % b
      decode = \case
            Imaginary -> True
            _         -> False

  assert $ decode (rsqrt x)

rsqrtRoundTrip :: Property
rsqrtRoundTrip = property $ do
  let numerators   = Gen.integral (Range.linear 0 100000)
  let denominators = Gen.integral (Range.linear 1 100000)

  a <- forAll numerators
  b <- forAll denominators

  let x = a % b
      f = square
      g = decode . rsqrt
      integerPart = a `div` b
      remainder = rem a b
      decode = \case
            Exactly       i -> i == integerPart && remainder == 0
            Approximately i -> i == integerPart && remainder > 0
            Imaginary       -> False

  assert $ g (f x)

square :: Rational -> Rational
square r =
  let
    n = numerator r
    d = denominator r
    two = 2 :: Integer
    in (n^two) % (d^two)

isqrtRoundTrip :: Property
isqrtRoundTrip = property $ do
  let positiveInteger = Gen.integral (Range.linear 0 100000)
  x' <- forAll positiveInteger
  tripping x' sq (decodeExact . isqrt)
    where
      sq x = x ^ (2 :: Integer)
      decodeExact (Exactly x) = Right x
      decodeExact s           = Left  s

serdeTests :: TestTree
serdeTests = testGroup "Data serialisation"
    [ testProperty "data round-trip" dataRoundTrip
    ]

dataRoundTrip :: Property
dataRoundTrip = property $ do
    dt :: Data <- forAll genData
    let res = deserialiseOrFail (serialise dt)
    annotateShow res

    -- Debugging info
    let ft = FlatTerm.toFlatTerm $ Serialise.encode dt
    annotateShow ft
    annotateShow $ FlatTerm.validFlatTerm ft
    assert (res == Right dt)

genData :: MonadGen m => m Data
genData =
    let st = Gen.subterm genData id
        positiveInteger = Gen.integral (Range.linear 0 100000)
        constructorArgList = Gen.list (Range.linear 0 50) st
        kvMapList = Gen.list (Range.linear 0 50) ((,) <$> st <*> st)
    in
    Gen.recursive Gen.choice
        [ I <$> Gen.integral (Range.linear (-100000) 100000)
        , B <$> Gen.bytes (Range.linear 0 64) ]
        [ Constr <$> positiveInteger <*> constructorArgList
        , List <$> constructorArgList
        , Map <$> kvMapList
        ]

ratioTests :: TestTree
ratioTests = testGroup "Ratio"
  [ testProperty "reciprocal ordering 1" reciprocalOrdering1
  , testProperty "reciprocal ordering 2" reciprocalOrdering2
  , testProperty "reciprocal ordering 3" reciprocalOrdering3
  ]

genPositiveRational :: Monad m => PropertyT m Rational
genPositiveRational = do
  a <- forAll . Gen.integral $ Range.linear 1 100000
  b <- forAll . Gen.integral $ Range.linear 1 100000
  return (a % b)

genNegativeRational :: Monad m => PropertyT m Rational
genNegativeRational = negate <$> genPositiveRational

-- If x and y are positive rational numbers and x < y then 1/y < 1/x
reciprocalOrdering1 :: Property
reciprocalOrdering1 = property $ do
  x <- genPositiveRational
  y <- genPositiveRational
  if x < y
  then assert (recip y < recip x)
  else if y < x
  then assert (recip x < recip y)
  else return ()

-- If x and y are negative rational numbers and x < y then 1/y < 1/x
reciprocalOrdering2 :: Property
reciprocalOrdering2 = property $ do
  x <- genNegativeRational
  y <- genNegativeRational
  if x < y
  then assert (recip y < recip x)
  else if y < x
  then assert (recip x < recip y)
  else return ()

-- If x is a negative rational number and y is a positive rational number
-- then 1/x < 1/y
reciprocalOrdering3 :: Property
reciprocalOrdering3 = property $ do
  x <- genNegativeRational
  y <- genPositiveRational
  assert (recip x < recip y)

powModIntegerTests :: TestTree
powModIntegerTests = testGroup "Integer modular exponentation tests"
  [ testProperty "2^3 % 5 = 3" powModIntegerPositiveCheck
  , testProperty "0^0 % 1 = 0" powModIntegerZeroBaseZeroExponentCheck
  , testProperty "-2^3 % 5 = 2" powModIntegerNegativeBaseCheck
  ]

powModIntegerPositiveCheck :: Property
powModIntegerPositiveCheck = property $ do
  let a :: Integer = 2
  let e :: Integer = 3
  let m :: Integer = 5
  let r :: Integer = 3

  assert (r == powModInteger a e m)

powModIntegerZeroBaseZeroExponentCheck :: Property
powModIntegerZeroBaseZeroExponentCheck = property $ do
  let a :: Integer = 0
  let e :: Integer = 0
  let m :: Integer = 1
  let r :: Integer = 0

  assert (r == powModInteger a e m)

powModIntegerNegativeBaseCheck :: Property
powModIntegerNegativeBaseCheck = property $ do
  let a :: Integer = -2
  let e :: Integer = 3
  let m :: Integer = 5
  let r :: Integer = 2

  assert (r == powModInteger a e m)

invertIntegerTests :: TestTree
invertIntegerTests = testGroup "Integer modular multiplicative inverse tests"
  [ testProperty "inverse 3 modulo 11 = 4" invertIntegerCheck
  ]

invertIntegerCheck :: Property
invertIntegerCheck = property $ do
  let a :: Integer = 3
  let m :: Integer = 11
  let r :: Integer = 4

  assert (r == invertInteger a m)

probablyPrimeTests :: TestTree
probablyPrimeTests = testGroup "Integer primality test tests"
  [ testProperty "num: 3 reps: 15 = 2" definitelyPrimeIntegerCheck
  , testProperty "num: 8 reps: 15 = 0" definitelyNotPrimeIntegerCheck
  ]

definitelyPrimeIntegerCheck :: Property
definitelyPrimeIntegerCheck = property $ do
  let n :: Integer = 3
  let l :: Integer = 15
  let r :: Integer = 2

  assert (r == probablyPrimeInteger n l)

definitelyNotPrimeIntegerCheck :: Property
definitelyNotPrimeIntegerCheck = property $ do
  let n :: Integer = 8
  let l :: Integer = 15
  let r :: Integer = 0

  assert (r == probablyPrimeInteger n l)
