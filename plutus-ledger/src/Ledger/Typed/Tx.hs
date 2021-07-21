{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wno-orphans           #-}

-- | Typed transaction inputs and outputs. This module defines typed versions
--   of various ledger types. The ultimate goal is to make sure that the script
--   types attached to inputs and outputs line up, to avoid type errors at
--   validation time.
module Ledger.Typed.Tx where

import           Ledger.Scripts
import           Ledger.Tx
import           Ledger.Typed.Scripts
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.TxId
import qualified Plutus.V1.Ledger.Value    as Value

import           PlutusTx

import           Codec.Serialise           (deserialise, serialise)
import qualified Data.ByteString.Lazy      as BSL

import           Data.Aeson                (FromJSON (..), ToJSON (..), Value (Object), object, (.:), (.=))
import           Data.Aeson.Types          (typeMismatch)
import           Data.Text.Prettyprint.Doc (Pretty (pretty), viaShow, (<+>))
import           GHC.Generics              (Generic)

import           Control.Monad.Except

-- | A 'TxIn' tagged by two phantom types: a list of the types of the data scripts in the transaction; and the connection type of the input.
data TypedScriptTxIn a = TypedScriptTxIn { tyTxInTxIn :: TxIn, tyTxInOutRef :: TypedScriptTxOutRef a }

instance Eq (DatumType a) => Eq (TypedScriptTxIn a) where
    l == r =
        tyTxInTxIn l == tyTxInTxIn r
        && tyTxInOutRef l == tyTxInOutRef r

instance (FromJSON (DatumType a), IsData (DatumType a)) => FromJSON (TypedScriptTxIn a) where
    parseJSON (Object v) =
        TypedScriptTxIn <$> v .: "tyTxInTxIn" <*> v .: "tyTxInOutRef"
    parseJSON invalid = typeMismatch "Object" invalid

instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxIn a) where
    toJSON TypedScriptTxIn{tyTxInTxIn, tyTxInOutRef} =
        object ["tyTxInTxIn" .= tyTxInTxIn, "tyTxInOutRef" .= tyTxInOutRef]

-- | Create a 'TypedScriptTxIn' from a correctly-typed validator, redeemer, and output ref.
makeTypedScriptTxIn
    :: forall inn
    . (IsData (RedeemerType inn), IsData (DatumType inn))
    => TypedValidator inn
    -> RedeemerType inn
    -> TypedScriptTxOutRef inn
    -> TypedScriptTxIn inn
makeTypedScriptTxIn si r tyRef@(TypedScriptTxOutRef ref TypedScriptTxOut{tyTxOutData=d}) =
    let vs = validatorScript si
        rs = Redeemer (toBuiltinData r)
        ds = Datum (toBuiltinData d)
        txInType = ConsumeScriptAddress vs rs ds
    in TypedScriptTxIn @inn (TxIn ref (Just txInType)) tyRef

txInValue :: TypedScriptTxIn a -> Value.Value
txInValue = txOutValue . tyTxOutTxOut . tyTxOutRefOut . tyTxInOutRef

-- | A public-key 'TxIn'. We need this to be sure that it is not a script input.
newtype PubKeyTxIn = PubKeyTxIn { unPubKeyTxIn :: TxIn }
    deriving stock (Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Create a 'PubKeyTxIn'.
makePubKeyTxIn :: TxOutRef -> PubKeyTxIn
makePubKeyTxIn ref = PubKeyTxIn . TxIn ref . Just $ ConsumePublicKeyAddress

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = IsData (DatumType a) => TypedScriptTxOut { tyTxOutTxOut :: TxOut, tyTxOutData :: DatumType a }

instance Eq (DatumType a) => Eq (TypedScriptTxOut a) where
    l == r =
        tyTxOutTxOut l == tyTxOutTxOut r
        && tyTxOutData l == tyTxOutData r

instance (FromJSON (DatumType a), IsData (DatumType a)) => FromJSON (TypedScriptTxOut a) where
    parseJSON (Object v) =
        TypedScriptTxOut <$> v .: "tyTxOutTxOut" <*> v .: "tyTxOutData"
    parseJSON invalid = typeMismatch "Object" invalid

instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxOut a) where
    toJSON TypedScriptTxOut{tyTxOutTxOut, tyTxOutData} =
        object ["tyTxOutTxOut" .= tyTxOutTxOut, "tyTxOutData" .= tyTxOutData]

-- | Create a 'TypedScriptTxOut' from a correctly-typed data script, an address, and a value.
makeTypedScriptTxOut
    :: forall out
    . (IsData (DatumType out))
    => TypedValidator out
    -> DatumType out
    -> Value.Value
    -> TypedScriptTxOut out
makeTypedScriptTxOut ct d value =
    let outTy = datumHash $ Datum $ toBuiltinData d
    in TypedScriptTxOut @out TxOut{txOutAddress = validatorAddress ct, txOutValue=value, txOutDatumHash = Just outTy} d

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef { tyTxOutRefRef :: TxOutRef, tyTxOutRefOut :: TypedScriptTxOut a }

instance Eq (DatumType a) => Eq (TypedScriptTxOutRef a) where
    l == r =
        tyTxOutRefRef l == tyTxOutRefRef r
        && tyTxOutRefOut l == tyTxOutRefOut r

instance (FromJSON (DatumType a), IsData (DatumType a)) => FromJSON (TypedScriptTxOutRef a) where
    parseJSON (Object v) =
        TypedScriptTxOutRef <$> v .: "tyTxOutRefRef" <*> v .: "tyTxOutRefOut"
    parseJSON invalid = typeMismatch "Object" invalid

instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxOutRef a) where
    toJSON TypedScriptTxOutRef{tyTxOutRefRef, tyTxOutRefOut} =
        object ["tyTxOutRefRef" .= tyTxOutRefRef, "tyTxOutRefOut" .= tyTxOutRefOut]

-- | A public-key 'TxOut'. We need this to be sure that it is not a script output.
newtype PubKeyTxOut = PubKeyTxOut { unPubKeyTxOut :: TxOut }
    deriving stock (Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Create a 'PubKeyTxOut'.
makePubKeyTxOut :: Value.Value -> PubKey -> PubKeyTxOut
makePubKeyTxOut value pubKey = PubKeyTxOut $ pubKeyTxOut value pubKey

data WrongOutTypeError =
    ExpectedScriptGotPubkey
    | ExpectedPubkeyGotScript
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError =
    WrongValidatorAddress Address Address
    | WrongOutType WrongOutTypeError
    | WrongInType TxInType
    | MissingInType
    | WrongValidatorType String
    | WrongRedeemerType BuiltinData
    | WrongDatumType BuiltinData
    | NoDatum TxId DatumHash
    | UnknownRef
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- TODO: these should probably live somewhere else
instance ToJSON BuiltinData where
    toJSON d = toJSON (BSL.toStrict (serialise (builtinDataToData d)))
instance FromJSON BuiltinData where
    parseJSON v = dataToBuiltinData . deserialise . BSL.fromStrict <$> parseJSON v

instance Pretty ConnectionError where
    pretty = \case
        WrongValidatorAddress a1 a2 -> "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
        WrongOutType t              -> "Wrong out type:" <+> viaShow t
        WrongInType t               -> "Wrong in type:" <+> viaShow t
        MissingInType               -> "Missing in type"
        WrongValidatorType t        -> "Wrong validator type:" <+> pretty t
        WrongRedeemerType d         -> "Wrong redeemer type" <+> pretty (builtinDataToData d)
        WrongDatumType d            -> "Wrong datum type" <+> pretty (builtinDataToData d)
        NoDatum t d                 -> "No datum with hash " <+> pretty d <+> "for tx" <+> pretty t
        UnknownRef                  -> "Unknown reference"

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress :: forall a m . (MonadError ConnectionError m) => TypedValidator a -> Address -> m ()
checkValidatorAddress ct actualAddr = do
    let expectedAddr = validatorAddress ct
    unless (expectedAddr == actualAddr) $ throwError $ WrongValidatorAddress expectedAddr actualAddr

-- | Checks that the given redeemer script has the right type.
checkRedeemer
    :: forall inn m
    . (IsData (RedeemerType inn), MonadError ConnectionError m)
    => TypedValidator inn
    -> Redeemer
    -> m (RedeemerType inn)
checkRedeemer _ (Redeemer d) =
    case fromBuiltinData d of
        Just v  -> pure v
        Nothing -> throwError $ WrongRedeemerType d

-- | Checks that the given datum has the right type.
checkDatum
    :: forall a m . (IsData (DatumType a), MonadError ConnectionError m)
    => TypedValidator a
    -> Datum
    -> m (DatumType a)
checkDatum _ (Datum d) =
    case fromBuiltinData d of
        Just v  -> pure v
        Nothing -> throwError $ WrongDatumType d

-- | Create a 'TypedScriptTxIn' from an existing 'TxIn' by checking the types of its parts.
typeScriptTxIn
    :: forall inn m
    . ( IsData (RedeemerType inn)
      , IsData (DatumType inn)
      , MonadError ConnectionError m)
    => (TxOutRef -> Maybe TxOutTx)
    -> TypedValidator inn
    -> TxIn
    -> m (TypedScriptTxIn inn)
typeScriptTxIn lookupRef si TxIn{txInRef,txInType} = do
    (rs, ds) <- case txInType of
        Just (ConsumeScriptAddress _ rs ds) -> pure (rs, ds)
        Just x                              -> throwError $ WrongInType x
        Nothing                             -> throwError MissingInType
    -- It would be nice to typecheck the validator script here (we used to do that when we
    -- had typed on-chain code), but we can't do that with untyped code!
    rsVal <- checkRedeemer si rs
    _ <- checkDatum si ds
    typedOut <- typeScriptTxOutRef @inn lookupRef si txInRef
    pure $ makeTypedScriptTxIn si rsVal typedOut

-- | Create a 'PubKeyTxIn' from an existing 'TxIn' by checking that it has the right payment type.
typePubKeyTxIn
    :: forall m
    . (MonadError ConnectionError m)
    => TxIn
    -> m PubKeyTxIn
typePubKeyTxIn inn@TxIn{txInType} = do
    case txInType of
        Just ConsumePublicKeyAddress -> pure ()
        Just x                       -> throwError $ WrongInType x
        Nothing                      -> throwError MissingInType

    pure $ PubKeyTxIn inn

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut
    :: forall out m
    . ( IsData (DatumType out)
      , MonadError ConnectionError m)
    => TypedValidator out
    -> TxOutTx
    -> m (TypedScriptTxOut out)
typeScriptTxOut si TxOutTx{txOutTxTx=tx, txOutTxOut=TxOut{txOutAddress,txOutValue,txOutDatumHash}} = do
    dsh <- case txOutDatumHash of
        Just ds -> pure ds
        _       -> throwError $ WrongOutType ExpectedScriptGotPubkey
    ds <- case lookupDatum tx dsh of
        Just ds -> pure ds
        Nothing -> throwError $ NoDatum (txId tx) dsh
    checkValidatorAddress si txOutAddress
    dsVal <- checkDatum si ds
    pure $ makeTypedScriptTxOut si dsVal txOutValue

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts. To do this we
-- need to cross-reference against the validator script and be able to look up the 'TxOut' to which this
-- reference points.
typeScriptTxOutRef
    :: forall out m
    . ( IsData (DatumType out)
      , MonadError ConnectionError m)
    => (TxOutRef -> Maybe TxOutTx)
    -> TypedValidator out
    -> TxOutRef
    -> m (TypedScriptTxOutRef out)
typeScriptTxOutRef lookupRef ct ref = do
    out <- case lookupRef ref of
        Just res -> pure res
        Nothing  -> throwError UnknownRef
    tyOut <- typeScriptTxOut @out ct out
    pure $ TypedScriptTxOutRef ref tyOut

-- | Create a 'PubKeyTxOUt' from an existing 'TxOut' by checking that it has the right payment type.
typePubKeyTxOut
    :: forall m
    . (MonadError ConnectionError m)
    => TxOut
    -> m PubKeyTxOut
typePubKeyTxOut out@TxOut{txOutDatumHash} = do
    case txOutDatumHash of
        Nothing -> pure ()
        Just _  -> throwError $ WrongOutType ExpectedPubkeyGotScript
    pure $ PubKeyTxOut out
