{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Temporary code that'll make it easy for us to generate arbitrary events.
-- This should either be deleted when we can get real events, or at least moved
-- across to the test suite.
module Plutus.PAB.Arbitrary where

import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson
import           Ledger                            (ValidatorHash (ValidatorHash))
import qualified Ledger
import           Ledger.Address                    (Address (..))
import           Ledger.Bytes                      (LedgerBytes)
import qualified Ledger.Bytes                      as LedgerBytes
import           Ledger.Crypto                     (PubKey, PubKeyHash, Signature)
import           Ledger.Interval                   (Extended, Interval, LowerBound, UpperBound)
import           Ledger.Slot                       (Slot)
import           Ledger.Tx                         (RedeemerPtr, ScriptTag, Tx, TxIn, TxInType, TxOut, TxOutRef)
import           Ledger.TxId                       (TxId)
import           Plutus.Contract.Effects           (ActiveEndpoint (..), PABReq (..), PABResp (..), Waited (..))
import qualified PlutusTx                          as PlutusTx
import qualified PlutusTx.AssocMap                 as AssocMap
import           Test.QuickCheck                   (Gen, oneof)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary, arbitrary, genericArbitrary, genericShrink, shrink)
import           Test.QuickCheck.Instances         ()
import           Wallet                            (WalletAPIError)
import           Wallet.Effects                    (AddressChangeRequest (..))
import           Wallet.Types                      (EndpointDescription (..), EndpointValue (..))

-- | A validator that always succeeds.
acceptingValidator :: Ledger.Validator
acceptingValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| (\_ _ _ -> ()) ||])

-- | A minting policy that always succeeds.
acceptingMintingPolicy :: Ledger.MintingPolicy
acceptingMintingPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [|| (\_ _ -> ()) ||])

instance Arbitrary LedgerBytes where
    arbitrary = LedgerBytes.fromBytes <$> arbitrary

instance Arbitrary Ledger.MintingPolicy where
    arbitrary = pure acceptingMintingPolicy

instance Arbitrary Ledger.MintingPolicyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.ValidationError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.ScriptError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletAPIError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Tx where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxIn where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxOut where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxOutRef where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxInType where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ScriptTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RedeemerPtr where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Value where
    arbitrary = oneof [Aeson.String <$> arbitrary, Aeson.Number <$> arbitrary]

instance Arbitrary a => Arbitrary (Extended a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (LowerBound a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (UpperBound a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (Interval a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PubKeyHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Slot where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Signature where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PlutusTx.Data where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PlutusTx.BuiltinData where
    arbitrary = PlutusTx.dataToBuiltinData <$> arbitrary
    shrink d = PlutusTx.dataToBuiltinData <$> shrink (PlutusTx.builtinDataToData d)

instance Arbitrary Ledger.Datum where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.DatumHash where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Redeemer where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Validator where
    arbitrary = pure acceptingValidator

instance Arbitrary Ledger.TokenName where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.CurrencySymbol where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ledger.Value where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arbitrary k, Arbitrary v) => Arbitrary (AssocMap.Map k v) where
    arbitrary = AssocMap.fromList <$> arbitrary

instance Arbitrary AddressChangeRequest where
    arbitrary =  AddressChangeRequest <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PABReq where
    arbitrary =
        oneof
            [ AwaitSlotReq <$> arbitrary
            , pure CurrentSlotReq
            , AwaitTxConfirmedReq <$> arbitrary
            , pure OwnContractInstanceIdReq
            , ExposeEndpointReq <$> arbitrary
            , UtxoAtReq <$> arbitrary
            , AddressChangeReq <$> arbitrary
            , pure $ OwnPublicKeyReq
            -- TODO This would need an Arbitrary Tx instance:
            -- , BalanceTxRequest <$> arbitrary
            -- , WriteBalancedTxRequest <$> arbitrary
            ]

instance Arbitrary Address where
    arbitrary = oneof [Ledger.pubKeyAddress <$> arbitrary, Ledger.scriptAddress <$> arbitrary]

instance Arbitrary ValidatorHash where
    arbitrary = ValidatorHash <$> arbitrary

instance Arbitrary EndpointDescription where
    arbitrary = EndpointDescription <$> arbitrary

instance Arbitrary ActiveEndpoint where
    arbitrary = ActiveEndpoint . EndpointDescription <$> arbitrary <*> arbitrary

-- Maintainer's note: These requests are deliberately excluded - some
-- problem with the arbitrary instances for the responses never
-- terminating.
--
-- Since we're not going to keep this code for long, I won't worry
-- about fixing it, but I'll leave the offending data there as a
-- warning sign around the rabbit hole:
-- bad :: [Gen ContractRequest]
-- bad =
--     [ BalanceTxRequest <$> arbitrary
--     , WriteBalancedTxRequest <$> arbitrary
--     , UtxoAtRequest <$> arbitrary
--     , AddressChangedAtRequest <$> arbitrary
--     ]

-- | Generate responses for mock requests. This function returns a
-- 'Maybe' because we can't (yet) create a generator for every request
-- type.
genResponse :: PABReq -> Maybe (Gen PABResp)
genResponse (AwaitSlotReq slot)        = Just . pure . AwaitSlotResp . Waited $ slot
genResponse (AwaitTxConfirmedReq txId) = Just . pure . AwaitTxConfirmedResp . Waited $ txId
genResponse (ExposeEndpointReq _)      = Just $ ExposeEndpointResp <$> arbitrary <*> (Waited . EndpointValue <$> arbitrary)
genResponse OwnPublicKeyReq            = Just $ OwnPublicKeyResp <$> arbitrary
genResponse _                          = Nothing
