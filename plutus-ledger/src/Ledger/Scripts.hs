{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
This module re-exports the module 'Plutus.V1.Ledger.Scripts', but with
additionnal functionality.

This module contains orphan instances of 'Cardano.Api.HasTextEnvelope', since
the Cardano Node CLI expects serialised binary values to be wrapped with a
'Cardano.Api.TextEnvelope'.
-}
module Ledger.Scripts (
    module Export
    , datumHash
    , redeemerHash
    , validatorHash
    , mintingPolicyHash
    ) where

import           Cardano.Api              (AsType, HasTextEnvelope (textEnvelopeType), HasTypeProxy (proxyToAsType),
                                           SerialiseAsCBOR, TextEnvelopeType (TextEnvelopeType))
import           Cardano.Binary           (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import qualified Cardano.Crypto.Hash      as Crypto
import           Codec.Serialise          (decode, encode, serialise)
import qualified Data.ByteArray           as BA
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as Text
import           Plutus.V1.Ledger.Api     (plutusDatumEnvelopeType, plutusRedeemerEnvelopeType,
                                           plutusScriptEnvelopeType)
import           Plutus.V1.Ledger.Scripts as Export
import           PlutusTx.Builtins        as Builtins

instance HasTextEnvelope Script where
    textEnvelopeType _ = TextEnvelopeType $ Text.unpack plutusScriptEnvelopeType

instance SerialiseAsCBOR Script

instance FromCBOR Script where
    fromCBOR = decode

instance ToCBOR Script where
    toCBOR = encode

instance HasTypeProxy Script where
    data AsType Script = AsScript
    proxyToAsType _ = AsScript

instance HasTextEnvelope Datum where
  textEnvelopeType _ = TextEnvelopeType $ Text.unpack plutusDatumEnvelopeType

instance SerialiseAsCBOR Datum

instance FromCBOR Datum where
    fromCBOR = decode

instance ToCBOR Datum where
    toCBOR = encode

instance HasTypeProxy Datum where
    data AsType Datum = AsDatum
    proxyToAsType _ = AsDatum

instance HasTextEnvelope Redeemer where
  textEnvelopeType _ = TextEnvelopeType $ Text.unpack plutusRedeemerEnvelopeType

instance SerialiseAsCBOR Redeemer

instance FromCBOR Redeemer where
    fromCBOR = decode

instance ToCBOR Redeemer where
    toCBOR = encode

instance HasTypeProxy Redeemer where
    data AsType Redeemer = AsRedeemer
    proxyToAsType _ = AsRedeemer

datumHash :: Datum -> DatumHash
datumHash = DatumHash . Builtins.sha2_256 . BA.convert

redeemerHash :: Redeemer -> RedeemerHash
redeemerHash = RedeemerHash . Builtins.sha2_256 . BA.convert

validatorHash :: Validator -> ValidatorHash
validatorHash vl =
    ValidatorHash
        $ Crypto.hashToBytes
        $ Crypto.hashWith @Crypto.Blake2b_224 id
        $ Crypto.hashToBytes
        $ Crypto.hashWith @Crypto.Blake2b_224 id
        $ BSL.toStrict
        $ serialise vl

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash vl =
    MintingPolicyHash
        $ Crypto.hashToBytes
        $ Crypto.hashWith @Crypto.Blake2b_224 id
        $ Crypto.hashToBytes
        $ Crypto.hashWith @Crypto.Blake2b_224 id
        $ BSL.toStrict
        $ serialise vl
