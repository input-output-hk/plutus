-- | Create the cost model from the CSV data and save it in data/costModel.json
module UpdateCostModel where

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BSL

import           CostModelCreation

{- See Note [Creation of the Cost Model]
-}
main :: IO ()
main = do
  model <- createCostModel
  BSL.writeFile "cost-model/data/costModel.json" $ encodePretty' (defConfig { confCompare = \_ _-> EQ }) model
