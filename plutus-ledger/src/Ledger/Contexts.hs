module Ledger.Contexts
    ( module Export
    , pubKeyHash
    , scriptCurrencySymbol
    ) where

import           Ledger.Crypto             (pubKeyHash)
import           Ledger.Scripts            (MintingPolicy, MintingPolicyHash (..), mintingPolicyHash)
import           Plutus.V1.Ledger.Contexts as Export
import           Plutus.V1.Ledger.Value    (CurrencySymbol (..))
import qualified Plutus.V1.Ledger.Value    as Value

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol scrpt = let (MintingPolicyHash hsh) = mintingPolicyHash scrpt in Value.currencySymbol hsh

