module Spec.Marlowe.Actus
    ( tests
    )
where

import           Spec.Marlowe.Util
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Actus" [ testCase fileName (staticFromFile fileName) | fileName <- testFileNames ]

testFileNames :: [String]
testFileNames =
  [
    "actus-tests-pam"
  ]

excludedTestCases :: [String]
excludedTestCases =
  [
    "pam25" -- dates include hours, minutes, seconds
  ]


pathToTestData :: IO String
pathToTestData = do
  getEnv "ACTUS_TEST_DATA_DIR"

staticFromFile :: String -> IO ()
staticFromFile fileName = do
  testDataPath <- pathToTestData
  assertTestResultsFromFile excludedTestCases (testDataPath ++ fileName ++ ".json")
