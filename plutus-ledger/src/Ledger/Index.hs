{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An index of unspent transaction outputs, and some functions for validating
--   transactions using the index.
module Ledger.Index(
    -- * Types for transaction validation based on UTXO index
    ValidationMonad,
    UtxoIndex(..),
    insert,
    insertCollateral,
    insertBlock,
    initialise,
    Validation,
    runValidation,
    lkpValue,
    lkpTxOut,
    lkpOutputs,
    ValidationError(..),
    ValidationErrorInPhase,
    ValidationPhase(..),
    InOutMatch(..),
    minFee,
    -- * Actual validation
    validateTransaction,
    validateTransactionOffChain,
    -- * Script validation events
    ScriptType(..),
    ScriptValidationEvent(..),
    Api.ExBudget(..),
    Api.ExCPU(..),
    Api.ExMemory(..),
    Api.SatInt
    ) where

import           Prelude                          hiding (lookup)


import           Codec.Serialise                  (Serialise)
import           Control.DeepSeq                  (NFData)
import           Control.Lens                     (toListOf, view, (^.))
import           Control.Lens.Indexed             (iforM_)
import           Control.Monad
import           Control.Monad.Except             (ExceptT, MonadError (..), runExcept, runExceptT)
import           Control.Monad.Reader             (MonadReader (..), ReaderT (..), ask)
import           Control.Monad.Writer             (MonadWriter, Writer, runWriter, tell)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default                     (Default (def))
import           Data.Foldable                    (asum, fold, foldl', traverse_)
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Data.Text.Prettyprint.Doc        (Pretty)
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           GHC.Generics                     (Generic)
import           Ledger.Blockchain
import qualified Ledger.TimeSlot                  as TimeSlot
import qualified Plutus.V1.Ledger.Ada             as Ada
import           Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Api             as Api
import           Plutus.V1.Ledger.Contexts        (ScriptContext (..), ScriptPurpose (..), TxInfo (..))
import qualified Plutus.V1.Ledger.Contexts        as Validation
import           Plutus.V1.Ledger.Credential      (Credential (..))
import           Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Interval        as Interval
import           Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import qualified Plutus.V1.Ledger.Slot            as Slot
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.TxId
import qualified Plutus.V1.Ledger.Value           as V
import           PlutusTx                         (toBuiltinData)
import qualified PlutusTx.Numeric                 as P

-- | Context for validating transactions. We need access to the unspent
--   transaction outputs of the blockchain, and we can throw 'ValidationError's.
type ValidationMonad m = (MonadReader UtxoIndex m, MonadError ValidationError m, MonadWriter [ScriptValidationEvent] m)

-- | The UTxOs of a blockchain indexed by their references.
newtype UtxoIndex = UtxoIndex { getIndex :: Map.Map TxOutRef TxOut }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Semigroup, Monoid, Serialise)
    deriving anyclass (FromJSON, ToJSON, NFData)

-- | Create an index of all UTxOs on the chain.
initialise :: Blockchain -> UtxoIndex
initialise = UtxoIndex . unspentOutputs

-- | Update the index for the addition of a transaction.
insert :: Tx -> UtxoIndex -> UtxoIndex
insert tx = UtxoIndex . updateUtxo tx . getIndex

-- | Update the index for the addition of only the collateral inputs of a failed transaction.
insertCollateral :: Tx -> UtxoIndex -> UtxoIndex
insertCollateral tx = UtxoIndex . updateUtxoCollateral tx . getIndex

-- | Update the index for the addition of a block.
insertBlock :: Block -> UtxoIndex -> UtxoIndex
insertBlock blck i = foldl' (flip (eitherTx insertCollateral insert)) i blck

-- | Find an unspent transaction output by the 'TxOutRef' that spends it.
lookup :: MonadError ValidationError m => TxOutRef -> UtxoIndex -> m TxOut
lookup i index = case Map.lookup i $ getIndex index of
    Just t  -> pure t
    Nothing -> throwError $ TxOutRefNotFound i

-- | A reason why a transaction is invalid.
data ValidationError =
    InOutTypeMismatch TxIn TxOut
    -- ^ A pay-to-pubkey output was consumed by a pay-to-script input or vice versa, or the 'TxIn' refers to a different public key than the 'TxOut'.
    | TxOutRefNotFound TxOutRef
    -- ^ The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | InvalidScriptHash Validator ValidatorHash
    -- ^ For pay-to-script outputs: the validator script provided in the transaction input does not match the hash specified in the transaction output.
    | InvalidDatumHash Datum DatumHash
    -- ^ For pay-to-script outputs: the datum provided in the transaction input does not match the hash specified in the transaction output.
    | MissingRedeemer RedeemerPtr
    -- ^ For scripts that take redeemers: no redeemer was provided for this script.
    | InvalidSignature PubKey Signature
    -- ^ For pay-to-pubkey outputs: the signature of the transaction input does not match the public key of the transaction output.
    | ValueNotPreserved V.Value V.Value
    -- ^ The amount spent by the transaction differs from the amount consumed by it.
    | NegativeValue Tx
    -- ^ The transaction produces an output with a negative value.
    | NonAdaFees Tx
    -- ^ The fee is not denominated entirely in Ada.
    | ScriptFailure ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | CurrentSlotOutOfRange Slot.Slot
    -- ^ The current slot is not covered by the transaction's validity slot range.
    | SignatureMissing PubKeyHash
    -- ^ The transaction is missing a signature
    | MintWithoutScript Scripts.MintingPolicyHash
    -- ^ The transaction attempts to mint value of a currency without running
    --   the currency's minting policy.
    | TransactionFeeTooLow V.Value V.Value
    -- ^ The transaction fee is lower than the minimum acceptable fee.
    deriving (Eq, Show, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError
deriving via (PrettyShow ValidationError) instance Pretty ValidationError

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show, Generic, FromJSON, ToJSON)
deriving via (PrettyShow ValidationPhase) instance Pretty ValidationPhase
type ValidationErrorInPhase = (ValidationPhase, ValidationError)

-- | A monad for running transaction validation inside, which is an instance of 'ValidationMonad'.
newtype Validation a = Validation { _runValidation :: (ReaderT UtxoIndex (ExceptT ValidationError (Writer [ScriptValidationEvent]))) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader UtxoIndex, MonadError ValidationError, MonadWriter [ScriptValidationEvent])

-- | Run a 'Validation' on a 'UtxoIndex'.
runValidation :: Validation (Maybe ValidationErrorInPhase, UtxoIndex) -> UtxoIndex -> ((Maybe ValidationErrorInPhase, UtxoIndex), [ScriptValidationEvent])
runValidation l idx = runWriter $ fmap (either (\e -> (Just (Phase1, e), idx)) id) $ runExceptT $ runReaderT (_runValidation l) idx

-- | Determine the unspent value that a ''TxOutRef' refers to.
lkpValue :: ValidationMonad m => TxOutRef -> m V.Value
lkpValue = fmap txOutValue . lkpTxOut

-- | Find an unspent transaction output by its reference. Assumes that the
--   output for this reference exists. If you want to handle the lookup error
--   you can use 'runLookup'.
lkpTxOut :: ValidationMonad m => TxOutRef -> m TxOut
lkpTxOut t = lookup t =<< ask

-- | Validate a transaction in a 'ValidationMonad' context.
validateTransaction :: ValidationMonad m
    => Slot.Slot
    -> Tx
    -> m (Maybe ValidationErrorInPhase, UtxoIndex)
validateTransaction h t = do
    -- Phase 1 validation
    checkSlotRange h t
    _ <- lkpOutputs $ toListOf (inputs . scriptTxIns) t

    -- see note [Minting of Ada]
    emptyUtxoSet <- reader (Map.null . getIndex)
    unless emptyUtxoSet (checkTransactionFee t)

    validateTransactionOffChain t

validateTransactionOffChain :: ValidationMonad m
    => Tx
    -> m (Maybe ValidationErrorInPhase, UtxoIndex)
validateTransactionOffChain t = do
    checkValuePreserved t
    checkPositiveValues t
    checkFeeIsAda t

    -- see note [Minting of Ada]
    emptyUtxoSet <- reader (Map.null . getIndex)
    unless emptyUtxoSet (checkMintingAuthorised t)

    checkValidInputs (toListOf (inputs . pubKeyTxIns)) t
    checkValidInputs (Set.toList . view collateralInputs) t

    (do
        -- Phase 2 validation
        checkValidInputs (toListOf (inputs . scriptTxIns)) t
        unless emptyUtxoSet (checkMintingScripts t)

        idx <- ask
        pure (Nothing, insert t idx)
        )
    `catchError` payCollateral
    where
        payCollateral e = do
            idx <- ask
            pure (Just (Phase2, e), insertCollateral t idx)

-- | Check that a transaction can be validated in the given slot.
checkSlotRange :: ValidationMonad m => Slot.Slot -> Tx -> m ()
checkSlotRange sl tx =
    if Interval.member sl (txValidRange tx)
    then pure ()
    else throwError $ CurrentSlotOutOfRange sl

-- | Check if the inputs of the transaction consume outputs that exist, and
--   can be unlocked by the signatures or validator scripts of the inputs.
checkValidInputs :: ValidationMonad m => (Tx -> [TxIn]) -> Tx -> m ()
checkValidInputs getInputs tx = do
    let tid = txId tx
        sigs = tx ^. signatures
    outs <- lkpOutputs (getInputs tx)
    matches <- traverse (uncurry (matchInputOutput tid sigs)) outs
    vld     <- mkTxInfo tx
    traverse_ (checkMatch vld) matches

-- | Match each input of the transaction with the output that it spends.
lkpOutputs :: ValidationMonad m => [TxIn] -> m [(TxIn, TxOut)]
lkpOutputs = traverse (\t -> traverse (lkpTxOut . txInRef) (t, t))

{- note [Minting of Ada]

'checkMintingAuthorised' will never allow a transaction that mints Ada.
Ada's currency symbol is the empty bytestring, and it can never be matched by a
validator script whose hash is its symbol.

Therefore 'checkMintingAuthorised' should not be applied to the first transaction in
the blockchain.

-}

-- | Check whether each currency minted by the transaction is matched by
--   a corresponding minting policy script (in the form of a pay-to-script
--   output of the currency's address).
--
checkMintingAuthorised :: ValidationMonad m => Tx -> m ()
checkMintingAuthorised tx =
    let
        mintedCurrencies = V.symbols (txMint tx)

        mpsScriptHashes = Scripts.MintingPolicyHash . V.unCurrencySymbol <$> mintedCurrencies

        lockingScripts = mintingPolicyHash <$> Set.toList (txMintScripts tx)

        mintedWithoutScript = filter (\c -> c `notElem` lockingScripts) mpsScriptHashes
    in
        traverse_ (throwError . MintWithoutScript) mintedWithoutScript

checkMintingScripts :: forall m . ValidationMonad m => Tx -> m ()
checkMintingScripts tx = do
    txinfo <- mkTxInfo tx
    iforM_ (Set.toList (txMintScripts tx)) $ \i vl -> do
        let cs :: V.CurrencySymbol
            cs = V.mpsSymbol $ mintingPolicyHash vl
            ctx :: Context
            ctx = Context $ toBuiltinData $ ScriptContext { scriptContextPurpose = Minting cs, scriptContextTxInfo = txinfo }
            ptr :: RedeemerPtr
            ptr = RedeemerPtr Mint (fromIntegral i)
        red <- case lookupRedeemer tx ptr of
            Just r  -> pure r
            Nothing -> throwError $ MissingRedeemer ptr

        case runExcept $ runMintingPolicyScript ctx vl red of
            Left e  -> do
                tell [mpsValidationEvent ctx vl red (Left e)]
                throwError $ ScriptFailure e
            res -> tell [mpsValidationEvent ctx vl red res]

-- | A matching pair of transaction input and transaction output, ensuring that they are of matching types also.
data InOutMatch =
    ScriptMatch
        TxOutRef
        Validator
        Redeemer
        Datum
    | PubKeyMatch TxId PubKey Signature
    deriving (Eq, Ord, Show)

-- | Match a transaction input with the output that it consumes, ensuring that
--   both are of the same type (pubkey or pay-to-script).
matchInputOutput :: ValidationMonad m
    => TxId
    -- ^ Hash of the transaction that is being verified
    -> Map.Map PubKey Signature
    -- ^ Signatures provided with the transaction
    -> TxIn
    -- ^ Input that allegedly spends the output
    -> TxOut
    -- ^ The unspent transaction output we are trying to unlock
    -> m InOutMatch
matchInputOutput txid mp txin txo = case (txInType txin, txOutDatumHash txo, txOutAddress txo) of
    (Just (ConsumeScriptAddress v r d), Just dh, Address{addressCredential=ScriptCredential vh}) -> do
        unless (datumHash d == dh) $ throwError $ InvalidDatumHash d dh
        unless (validatorHash v == vh) $ throwError $ InvalidScriptHash v vh

        pure $ ScriptMatch (txInRef txin) v r d
    (Just ConsumePublicKeyAddress, Nothing, Address{addressCredential=PubKeyCredential pkh}) ->
        let sigMatches = flip fmap (Map.toList mp) $ \(pk,sig) ->
                if pubKeyHash pk == pkh
                then Just (PubKeyMatch txid pk sig)
                else Nothing
        in case asum sigMatches of
            Just m  -> pure m
            Nothing -> throwError $ SignatureMissing pkh
    _ -> throwError $ InOutTypeMismatch txin txo

-- | Check that a matching pair of transaction input and transaction output is
--   valid. If this is a pay-to-script output then the script hash needs to be
--   correct and script evaluation has to terminate successfully. If this is a
--   pay-to-pubkey output then the signature needs to match the public key that
--   locks it.
checkMatch :: ValidationMonad m => TxInfo -> InOutMatch -> m ()
checkMatch txinfo = \case
    ScriptMatch txOutRef vl r d -> do
        let
            ptx' = ScriptContext { scriptContextTxInfo = txinfo, scriptContextPurpose = Spending txOutRef }
            vd = Context (toBuiltinData ptx')
        case runExcept $ runScript vd vl d r of
            Left e -> do
                tell [validatorScriptValidationEvent vd vl d r (Left e)]
                throwError $ ScriptFailure e
            res -> tell [validatorScriptValidationEvent vd vl d r res]
    PubKeyMatch msg pk sig -> unless (signedBy sig pk msg) $ throwError $ InvalidSignature pk sig

-- | Check if the value produced by a transaction equals the value consumed by it.
checkValuePreserved :: ValidationMonad m => Tx -> m ()
checkValuePreserved t = do
    inVal <- (P.+) (txMint t) <$> fmap fold (traverse (lkpValue . txInRef) (Set.toList $ view inputs t))
    let outVal = txFee t P.+ foldMap txOutValue (txOutputs t)
    if outVal == inVal
    then pure ()
    else throwError $ ValueNotPreserved inVal outVal

-- | Check if all values produced and consumed by a transaction are non-negative.
checkPositiveValues :: ValidationMonad m => Tx -> m ()
checkPositiveValues t =
    if validValuesTx t
    then pure ()
    else throwError $ NegativeValue t

-- | Check if the fees are paid exclusively in Ada.
checkFeeIsAda :: ValidationMonad m => Tx -> m ()
checkFeeIsAda t =
    if (Ada.toValue $ Ada.fromValue $ txFee t) == txFee t
    then pure ()
    else throwError $ NonAdaFees t

-- | Minimum transaction fee.
minFee :: Tx -> V.Value
minFee = const (Ada.lovelaceValueOf 10)

-- | Check that transaction fee is bigger than the minimum fee.
--   Skip the check on the first transaction (no inputs).
checkTransactionFee :: ValidationMonad m => Tx -> m ()
checkTransactionFee tx =
    if minFee tx `V.leq` txFee tx
    then pure ()
    else throwError $ TransactionFeeTooLow (txFee tx) (minFee tx)

-- | Create the data about the transaction which will be passed to a validator script.
mkTxInfo :: ValidationMonad m => Tx -> m TxInfo
mkTxInfo tx = do
    txins <- traverse mkIn $ Set.toList $ view inputs tx
    let ptx = TxInfo
            { txInfoInputs = txins
            , txInfoOutputs = txOutputs tx
            , txInfoForge = txMint tx
            , txInfoFee = txFee tx
            , txInfoDCert = [] -- DCerts not supported in emulator
            , txInfoWdrl = [] -- Withdrawals not supported in emulator
            , txInfoValidRange = TimeSlot.slotRangeToPOSIXTimeRange def $ txValidRange tx
            , txInfoSignatories = fmap pubKeyHash $ Map.keys (tx ^. signatures)
            , txInfoData = Map.toList (tx ^. datumWitnesses)
            , txInfoId = txId tx
            }
    pure ptx

-- | Create the data about a transaction input which will be passed to a validator script.
mkIn :: ValidationMonad m => TxIn -> m Validation.TxInInfo
mkIn TxIn{txInRef} = do
    txOut <- lkpTxOut txInRef
    pure $ Validation.TxInInfo{Validation.txInInfoOutRef = txInRef, Validation.txInInfoResolved=txOut}

data ScriptType = ValidatorScript | MintingPolicyScript
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A script (MPS or validator) that was run during transaction validation
data ScriptValidationEvent =
    ScriptValidationEvent
        { sveScript :: Script -- ^ The script applied to all arguments
        , sveResult :: Either ScriptError (Api.ExBudget, [String]) -- ^ Result of running the script: an error or the 'ExBudget' and trace logs
        , sveType   :: ScriptType -- ^ What type of script it was
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

validatorScriptValidationEvent
    :: Context
    -> Validator
    -> Datum
    -> Redeemer
    -> Either ScriptError (Api.ExBudget, [String])
    -> ScriptValidationEvent
validatorScriptValidationEvent ctx validator datum redeemer result =
    ScriptValidationEvent
        { sveScript = applyValidator ctx validator datum redeemer
        , sveResult = result
        , sveType = ValidatorScript
        }

mpsValidationEvent
    :: Context
    -> MintingPolicy
    -> Redeemer
    -> Either ScriptError (Api.ExBudget, [String])
    -> ScriptValidationEvent
mpsValidationEvent ctx mps red result =
    ScriptValidationEvent
        { sveScript = applyMintingPolicyScript ctx mps red
        , sveResult = result
        , sveType = MintingPolicyScript
        }
