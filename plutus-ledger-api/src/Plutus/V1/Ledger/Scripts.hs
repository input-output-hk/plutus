{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | Functions for working with scripts on the ledger.
module Plutus.V1.Ledger.Scripts(
    -- * Scripts
    Script (..),
    scriptSize,
    fromCompiledCode,
    ScriptError (..),
    evaluateScript,
    runScript,
    runMintingPolicyScript,
    applyValidator,
    applyMintingPolicyScript,
    mkTermToEvaluate,
    applyArguments,
    -- * Script wrappers
    mkValidatorScript,
    Validator (..),
    unValidatorScript,
    Redeemer(..),
    Datum(..),
    mkMintingPolicyScript,
    MintingPolicy (..),
    unMintingPolicyScript,
    Context(..),
    -- * Hashes
    DatumHash(..),
    RedeemerHash(..),
    ValidatorHash(..),
    MintingPolicyHash (..),
    -- * Example scripts
    unitRedeemer,
    unitDatum,
    ) where

import qualified Prelude                                  as Haskell

import           Codec.CBOR.Decoding                      (decodeBytes)
import           Codec.Serialise                          (Serialise, decode, encode, serialise)
import           Control.DeepSeq                          (NFData)
import           Control.Monad.Except                     (MonadError, runExceptT, throwError)
import           Data.Aeson                               (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson                               as JSON
import qualified Data.Aeson.Extras                        as JSON
import qualified Data.ByteArray                           as BA
import qualified Data.ByteString.Lazy                     as BSL
import           Data.Hashable                            (Hashable)
import           Data.String
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Extras
import qualified Flat
import           GHC.Generics                             (Generic)
import           Plutus.V1.Ledger.Bytes                   (LedgerBytes (..))
import           Plutus.V1.Ledger.Orphans                 ()
import qualified PlutusCore                               as PLC
import qualified PlutusCore.Data                          as PLC
import qualified PlutusCore.DeBruijn                      as PLC
import qualified PlutusCore.Evaluation.Machine.ExBudget   as PLC
import qualified PlutusCore.MkPlc                         as PLC
import           PlutusTx                                 (CompiledCode, IsData (..), getPlc, makeLift)
import           PlutusTx.Builtins                        as Builtins
import           PlutusTx.Builtins.Internal               as BI
import           PlutusTx.Evaluation                      (ErrorWithCause (..), EvaluationError (..), evaluateCekTrace)
import           PlutusTx.Prelude
import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

-- | A script on the chain. This is an opaque type as far as the chain is concerned.
newtype Script = Script { unScript :: UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun () }
  deriving stock Generic

{-| Note [Using Flat inside CBOR instance of Script]
`plutus-ledger` uses CBOR for data serialisation and `plutus-core` uses Flat. The
choice to use Flat was made to have a more efficient (most wins are in uncompressed
size) data serialisation format and use less space on-chain.

To make `plutus-ledger` work with scripts serialised with Flat, and keep the CBOR
format otherwise we have defined a Serialise instance for Script, which is a wrapper
over Programs serialised with Flat. The instance will see programs as an opaque
ByteString, which is the result of encoding programs using Flat.

Because Flat is not self-describing and it gets used in the encoding of Programs,
data structures that include scripts (for example, transactions) no-longer benefit
for CBOR's ability to self-describe it's format.
-}
instance Serialise Script where
  encode = encode . Flat.flat . unScript
  decode = do
    bs <- decodeBytes
    case Flat.unflat bs of
      Left  err    -> Haskell.fail (Haskell.show err)
      Right script -> return $ Script script

{- Note [Eq and Ord for Scripts]
We need `Eq` and `Ord` instances for `Script`s mostly so we can put them in `Set`s.
However, the `Eq` instance for `Program`s is *alpha-equivalence*, and we don't
have a compatible `Ord` instance, nor is it easy to derive one.

So we piggyback off a different representation. In this instance we have two
options:
- Use the serialized form
- Use a hash
The problem with the latter is that we don't want to add a derived `Hashable` instance
for `Program`s that's not compatible with the `Eq` instance. We *could* add a derived
instance for `Program`s with de Bruijn indices, since in that case the derived `Eq`
coincides with alpha-equivalence. However, this might be faster.

For the moment we use the serialized form. We used to store the serialized form directly
in `Script`, but that led to a lot of deserializing and reserializing in `applyProgram`.
Here we have to serialize when we do `Eq` or `Ord` operations, but this happens comparatively
infrequently (I believe).
-}
instance Eq Script where
    {-# INLINABLE (==) #-}
    a == b = BSL.toStrict (serialise a) == BSL.toStrict (serialise b)

instance Haskell.Eq Script where
    a == b = BSL.toStrict (serialise a) == BSL.toStrict (serialise b)

instance Ord Script where
    {-# INLINABLE compare #-}
    a `compare` b = BSL.toStrict (serialise a) `compare` BSL.toStrict (serialise b)

instance Haskell.Ord Script where
    a `compare` b = BSL.toStrict (serialise a) `compare` BSL.toStrict (serialise b)

instance Haskell.Show Script where
    showsPrec _ _ = Haskell.showString "<Script>"

instance NFData Script

-- | The size of a 'Script'. No particular interpretation is given to this, other than that it is
-- proportional to the serialized size of the script.
scriptSize :: Script -> Integer
scriptSize (Script s) = UPLC.programSize s

-- See Note [Normalized types in Scripts]
-- | Turn a 'CompiledCode' (usually produced by 'compile') into a 'Script' for use with this package.
fromCompiledCode :: CompiledCode a -> Script
fromCompiledCode = fromPlc . getPlc

fromPlc :: UPLC.Program UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () -> Script
fromPlc (UPLC.Program a v t) =
    let nameless = UPLC.termMapNames UPLC.unNameDeBruijn t
    in Script $ UPLC.Program a v nameless

data ScriptError =
    EvaluationError [Haskell.String] -- ^ Expected behavior of the engine (e.g. user-provided error)
    | EvaluationException Haskell.String -- ^ Unexpected behavior of the engine (a bug)
    | MalformedScript Haskell.String -- ^ Script is wrong in some way
    deriving (Haskell.Show, Haskell.Eq, Generic, NFData)
    deriving anyclass (ToJSON, FromJSON)

applyArguments :: Script -> [PLC.Data] -> Script
applyArguments (Script (UPLC.Program a v t)) args =
    let termArgs = Haskell.fmap (UPLC.termMapNames UPLC.unNameDeBruijn . PLC.mkConstant ()) args
        applied = PLC.mkIterApp () t termArgs
    in Script (UPLC.Program a v applied)

mkTermToEvaluate :: Script -> Either PLC.FreeVariableError (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun ())
mkTermToEvaluate (Script (UPLC.Program a v t)) =
    -- TODO: evaluate the nameless debruijn program directly
    let named = UPLC.termMapNames PLC.fakeNameDeBruijn t
        namedProgram = UPLC.Program a v named
    in PLC.runQuote $ runExceptT @PLC.FreeVariableError $ UPLC.unDeBruijnProgram namedProgram

-- | Evaluate a script, returning the trace log.
evaluateScript :: forall m . (MonadError ScriptError m) => Script -> m (PLC.ExBudget, [Haskell.String])
evaluateScript s = do
    p <- case mkTermToEvaluate s of
        Right p -> return p
        Left e  -> throwError $ MalformedScript $ Haskell.show e
    let (logOut, UPLC.TallyingSt _ budget, result) = evaluateCekTrace p
    case result of
        Right _ -> Haskell.pure ()
        Left errWithCause@(ErrorWithCause err _) -> throwError $ case err of
            InternalEvaluationError {} -> EvaluationException $ Haskell.show errWithCause
            UserEvaluationError {}     -> EvaluationError logOut -- TODO fix this error channel fuckery
    Haskell.pure (budget, logOut)

instance ToJSON Script where
    toJSON = JSON.String . JSON.encodeSerialise

instance FromJSON Script where
    parseJSON = JSON.decodeSerialise

instance ToJSON PLC.Data where
    toJSON = JSON.String . JSON.encodeSerialise

instance FromJSON PLC.Data where
    parseJSON = JSON.decodeSerialise

mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
mkValidatorScript = Validator . fromCompiledCode

unValidatorScript :: Validator -> Script
unValidatorScript = getValidator

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy
mkMintingPolicyScript = MintingPolicy . fromCompiledCode

unMintingPolicyScript :: MintingPolicy -> Script
unMintingPolicyScript = getMintingPolicy

-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator { getValidator :: Script }
  deriving stock (Generic)
  deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Serialise)
  deriving anyclass (ToJSON, FromJSON, NFData)
  deriving Pretty via (PrettyShow Validator)

instance Haskell.Show Validator where
    show = const "Validator { <script> }"

instance BA.ByteArrayAccess Validator where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

-- | 'Datum' is a wrapper around 'Data' values which are used as data in transaction outputs.
newtype Datum = Datum { getDatum :: BuiltinData  }
  deriving stock (Generic, Haskell.Show)
  deriving newtype (Haskell.Eq, Haskell.Ord, Eq, IsData)
  deriving (ToJSON, FromJSON, Serialise, NFData) via PLC.Data
  deriving Pretty via PLC.Data

instance BA.ByteArrayAccess Datum where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

-- | 'Redeemer' is a wrapper around 'Data' values that are used as redeemers in transaction inputs.
newtype Redeemer = Redeemer { getRedeemer :: BuiltinData }
  deriving stock (Generic, Haskell.Show)
  deriving newtype (Haskell.Eq, Haskell.Ord, Eq)
  deriving (ToJSON, FromJSON, Serialise, NFData, Pretty) via PLC.Data

instance BA.ByteArrayAccess Redeemer where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy { getMintingPolicy :: Script }
  deriving stock (Generic)
  deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Serialise)
  deriving anyclass (ToJSON, FromJSON, NFData)
  deriving Pretty via (PrettyShow MintingPolicy)

instance Haskell.Show MintingPolicy where
    show = const "MintingPolicy { <script> }"

instance BA.ByteArrayAccess MintingPolicy where
    length =
        BA.length . BSL.toStrict . serialise
    withByteArray =
        BA.withByteArray . BSL.toStrict . serialise

-- | Script runtime representation of a @Digest SHA256@.
newtype ValidatorHash =
    ValidatorHash Builtins.ByteString
    deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, IsData)
    deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey, NFData)

-- | Script runtime representation of a @Digest SHA256@.
newtype DatumHash =
    DatumHash Builtins.ByteString
    deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, IsData, NFData)
    deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey)

-- | Script runtime representation of a @Digest SHA256@.
newtype RedeemerHash =
    RedeemerHash Builtins.ByteString
    deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, IsData)
    deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey)

-- | Script runtime representation of a @Digest SHA256@.
newtype MintingPolicyHash =
    MintingPolicyHash Builtins.ByteString
    deriving (IsString, Haskell.Show, Serialise, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, Hashable, IsData)
    deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey)

-- | Information about the state of the blockchain and about the transaction
--   that is currently being validated, represented as a value in 'Data'.
newtype Context = Context BuiltinData
    deriving (ToJSON, FromJSON, Pretty, Haskell.Show) via PLC.Data

-- | Apply a 'Validator' to its 'Context', 'Datum', and 'Redeemer'.
applyValidator
    :: Context
    -> Validator
    -> Datum
    -> Redeemer
    -> Script
applyValidator (Context (BuiltinData valData)) (Validator validator) (Datum (BuiltinData datum)) (Redeemer (BuiltinData redeemer)) =
    applyArguments validator [datum, redeemer, valData]

-- | Evaluate a 'Validator' with its 'Context', 'Datum', and 'Redeemer', returning the log.
runScript
    :: (MonadError ScriptError m)
    => Context
    -> Validator
    -> Datum
    -> Redeemer
    -> m (PLC.ExBudget, [Haskell.String])
runScript context validator datum redeemer = do
    evaluateScript (applyValidator context validator datum redeemer)

-- | Apply 'MintingPolicy' to its 'Context' and 'Redeemer'.
applyMintingPolicyScript
    :: Context
    -> MintingPolicy
    -> Redeemer
    -> Script
applyMintingPolicyScript (Context (BuiltinData valData)) (MintingPolicy validator) (Redeemer (BuiltinData red)) =
    applyArguments validator [red, valData]

-- | Evaluate a 'MintingPolicy' with its 'Context' and 'Redeemer', returning the log.
runMintingPolicyScript
    :: (MonadError ScriptError m)
    => Context
    -> MintingPolicy
    -> Redeemer
    -> m (PLC.ExBudget, [Haskell.String])
runMintingPolicyScript context mps red = do
    evaluateScript (applyMintingPolicyScript context mps red)

-- | @()@ as a datum.
unitDatum :: Datum
unitDatum = Datum $ toBuiltinData ()

-- | @()@ as a redeemer.
unitRedeemer :: Redeemer
unitRedeemer = Redeemer $ toBuiltinData ()

makeLift ''ValidatorHash

makeLift ''DatumHash

makeLift ''MintingPolicyHash

makeLift ''RedeemerHash

makeLift ''Datum
