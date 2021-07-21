{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Ledger.Oracle(
  -- * Signed messages
  -- $oracles
  --
  Observation(..)
  , SignedMessage(..)
  -- * Checking signed messages
  , SignedMessageCheckError(..)
  , checkSignature
  , checkHashConstraints
  , checkHashOffChain
  , verifySignedMessageOffChain
  , verifySignedMessageOnChain
  , verifySignedMessageConstraints
  -- * Signing messages
  , signMessage
  , signObservation
  ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)

import           PlutusTx
import           PlutusTx.Prelude

import           Ledger.Constraints        (TxConstraints)
import qualified Ledger.Constraints        as Constraints
import           Ledger.Crypto             (PrivateKey, PubKey (..), Signature (..))
import qualified Ledger.Crypto             as Crypto
import           Ledger.Scripts            (Datum (..), DatumHash (..))
import qualified Ledger.Scripts            as Scripts
import           Plutus.V1.Ledger.Bytes
import           Plutus.V1.Ledger.Contexts (ScriptContext)
import           Plutus.V1.Ledger.Time     (POSIXTime)

import qualified Prelude                   as Haskell

-- $oracles
-- This module provides a way to verify signed messages, and a type for
--  observations (for example, the price of a commodity on a given date).
--  Together, the two can be used to implement trusted oracles:
--
--  * The oracle observes a value, for example 'Price' and constructs a value
--    @o = @ 'Observation' @Price@.
--  * The oracle then calls 'signMessage' @o pk@ with its own private key to
--    produce a 'SignedMessage' @(@'Observation' @Price)@.
--  * The signed message is passed to the contract as the redeemer of some
--    unspent output. __Important:__ The redeeming transaction must include the
--    message 'o' as a datum. This is because we can't hash anything in
--    on-chain code, and therefore have to rely on the node to do it for us
--    via the pending transaction's map of datum hashes to datums.
--    (The constraints resolution mechanism takes care of including the message)
--  * The contract then calls 'checkSignature' to check the signature, and
--    produces a constraint ensuring that the signed hash is really the hash
--    of the datum.

-- | A value that was observed at a specific point in time
data Observation a = Observation
    { obsValue :: a
    -- ^ The value
    , obsTime  :: POSIXTime
    -- ^ The time at which the value was observed
    } deriving (Generic, Haskell.Show)

instance Eq a => Eq (Observation a) where
    l == r =
        obsValue l == obsValue r
        && obsTime l == obsTime r

-- | @SignedMessage a@ contains the signature of a hash of a 'Datum'.
--   The 'Datum' can be decoded to a value of type @a@.
data SignedMessage a = SignedMessage
    { osmSignature   :: Signature
    -- ^ Signature of the message
    , osmMessageHash :: DatumHash
    -- ^ Hash of the message
    , osmDatum       :: Datum
    }
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

data SignedMessageCheckError =
    SignatureMismatch Signature PubKey DatumHash
    -- ^ The signature did not match the public key
    | DatumMissing DatumHash
    -- ^ The datum was missing from the pending transaction
    | DecodingError
    -- ^ The datum had the wrong shape
    | DatumNotEqualToExpected
    -- ^ The datum that correponds to the hash is wrong
    deriving (Generic, Haskell.Show)

{-# INLINABLE checkSignature #-}
-- | Verify the signature on a signed datum hash
checkSignature
  :: DatumHash
  -- ^ The hash of the message
  -> PubKey
  -- ^ The public key of the signatory
  -> Signature
  -- ^ The signed message
  -> Either SignedMessageCheckError ()
checkSignature datumHash pubKey signature_ =
    let PubKey (LedgerBytes pk) = pubKey
        Signature sig = signature_
        DatumHash h = datumHash
    in if verifySignature pk h sig
        then Right ()
        else Left $ SignatureMismatch signature_ pubKey datumHash

{-# INLINABLE checkHashConstraints #-}
-- | Extrat the contents of the message and produce a constraint that checks
--   that the hash is correct. In off-chain code, where we check the hash
--   straightforwardly, 'checkHashOffChain' can be used instead of this.
checkHashConstraints ::
    ( IsData a )
    => SignedMessage a
    -- ^ The signed message
    -> Either SignedMessageCheckError (a, TxConstraints i o)
checkHashConstraints SignedMessage{osmMessageHash, osmDatum=Datum dt} =
    maybe
        (trace "DecodingError" $ Left DecodingError)
        (\a -> pure (a, Constraints.mustHashDatum osmMessageHash (Datum dt)))
        (fromBuiltinData dt)

{-# INLINABLE verifySignedMessageConstraints #-}
-- | Check the signature on a 'SignedMessage' and extract the contents of the
--   message, producing a 'TxConstraint' value that ensures the hashes match
--   up.
verifySignedMessageConstraints ::
    ( IsData a)
    => PubKey
    -> SignedMessage a
    -> Either SignedMessageCheckError (a, TxConstraints i o)
verifySignedMessageConstraints pk s@SignedMessage{osmSignature, osmMessageHash} =
    checkSignature osmMessageHash pk osmSignature
    >> checkHashConstraints s

{-# INLINABLE verifySignedMessageOnChain #-}
-- | Check the signature on a 'SignedMessage' and extract the contents of the
--   message, using the pending transaction in lieu of a hash function. See
--   'verifySignedMessageConstraints' for a version that does not require a
--   'ScriptContext' value.
verifySignedMessageOnChain ::
    ( IsData a)
    => ScriptContext
    -> PubKey
    -> SignedMessage a
    -> Either SignedMessageCheckError a
verifySignedMessageOnChain ptx pk s@SignedMessage{osmSignature, osmMessageHash} = do
    checkSignature osmMessageHash pk osmSignature
    (a, constraints) <- checkHashConstraints s
    unless (Constraints.checkScriptContext @() @() constraints ptx)
        (Left $ DatumMissing osmMessageHash)
    pure a

-- | The off-chain version of 'checkHashConstraints', using the hash function
--   directly instead of obtaining the hash from a 'ScriptContext' value
checkHashOffChain ::
    ( IsData a )
    => SignedMessage a
    -> Either SignedMessageCheckError a
checkHashOffChain SignedMessage{osmMessageHash, osmDatum=dt} = do
    unless (osmMessageHash == Scripts.datumHash dt) (Left DatumNotEqualToExpected)
    let Datum dv = dt
    maybe (Left DecodingError) pure (fromBuiltinData dv)

-- | Check the signature on a 'SignedMessage' and extract the contents of the
--   message.
verifySignedMessageOffChain ::
    ( IsData a)
    => PubKey
    -> SignedMessage a
    -> Either SignedMessageCheckError a
verifySignedMessageOffChain pk s@SignedMessage{osmSignature, osmMessageHash} =
    checkSignature osmMessageHash pk osmSignature
    >> checkHashOffChain s

-- | Encode a message of type @a@ as a @Data@ value and sign the
--   hash of the datum.
signMessage :: IsData a => a -> PrivateKey -> SignedMessage a
signMessage msg pk =
  let dt = Datum (toBuiltinData msg)
      DatumHash msgHash = Scripts.datumHash dt
      sig     = Crypto.sign msgHash pk
  in SignedMessage
        { osmSignature = sig
        , osmMessageHash = DatumHash msgHash
        , osmDatum = dt
        }

-- | Encode an observation of a value of type @a@ that was made at the given time
signObservation :: IsData a => POSIXTime -> a -> PrivateKey -> SignedMessage (Observation a)
signObservation time vl = signMessage Observation{obsValue=vl, obsTime=time}

makeLift ''SignedMessage
makeIsDataIndexed ''SignedMessage [('SignedMessage,0)]

makeLift ''Observation
makeIsDataIndexed ''Observation [('Observation,0)]
