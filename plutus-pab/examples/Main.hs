{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import           ContractExample                     (ExampleContracts)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @ExampleContracts)
