{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Plutus.Contract.Effects( -- TODO: Move to Requests.Internal
    PABReq(..),
    _GetSlotConfigReq,
    _AwaitSlotReq,
    _AwaitTimeReq,
    _CurrentSlotReq,
    _CurrentTimeReq,
    _AwaitTxConfirmedReq,
    _OwnContractInstanceIdReq,
    _OwnPublicKeyReq,
    _UtxoAtReq,
    _AddressChangeReq,
    _BalanceTxReq,
    _WriteBalancedTxReq,
    _ExposeEndpointReq,
    PABResp(..),
    _GetSlotConfigResp,
    _AwaitSlotResp,
    _AwaitTimeResp,
    _CurrentSlotResp,
    _CurrentTimeResp,
    _AwaitTxConfirmedResp,
    _OwnContractInstanceIdResp,
    _OwnPublicKeyResp,
    _UtxoAtResp,
    _AddressChangeResp,
    _BalanceTxResp,
    _WriteBalancedTxResp,
    _ExposeEndpointResp,
    matches,

    -- * Etc.
    UtxoAtAddress(..),
    BalanceTxResponse(..),
    balanceTxResponse,
    WriteBalancedTxResponse(..),
    writeBalancedTxResponse,
    ActiveEndpoint(..),
    TxConfirmed(..)
    ) where

import           Control.Lens                (Iso', iso, makePrisms)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Aeson                  as JSON
import qualified Data.Map                    as Map
import           Data.Text.Prettyprint.Doc   (Pretty (..), colon, indent, viaShow, vsep, (<+>))
import           GHC.Generics                (Generic)
import           Ledger                      (Address, PubKey, Tx, TxId, TxOutTx (..), txId)
import           Ledger.AddressMap           (UtxoMap)
import           Ledger.Constraints.OffChain (UnbalancedTx)
import           Ledger.Slot                 (Slot (..))
import           Ledger.Time                 (POSIXTime (..))
import           Ledger.TimeSlot             (SlotConfig)
import           Wallet.API                  (WalletAPIError)
import           Wallet.Types                (AddressChangeRequest, AddressChangeResponse, ContractInstanceId,
                                              EndpointDescription, EndpointValue)

-- | Requests that 'Contract's can make
data PABReq =
    GetSlotConfigReq
    | AwaitSlotReq Slot
    | AwaitTimeReq POSIXTime
    | CurrentSlotReq
    | CurrentTimeReq
    | AwaitTxConfirmedReq TxId
    | OwnContractInstanceIdReq
    | OwnPublicKeyReq
    | UtxoAtReq Address
    | AddressChangeReq AddressChangeRequest
    | BalanceTxReq UnbalancedTx
    | WriteBalancedTxReq Tx
    | ExposeEndpointReq ActiveEndpoint
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty PABReq where
  pretty = \case
    GetSlotConfigReq         -> "Slot config"
    AwaitSlotReq s           -> "Await slot:" <+> pretty s
    AwaitTimeReq s           -> "Await time:" <+> pretty s
    CurrentSlotReq           -> "Current slot"
    CurrentTimeReq           -> "Current time"
    AwaitTxConfirmedReq txid -> "Await tx confirmed:" <+> pretty txid
    OwnContractInstanceIdReq -> "Own contract instance ID"
    OwnPublicKeyReq          -> "Own public key"
    UtxoAtReq addr           -> "Utxo at:" <+> pretty addr
    AddressChangeReq req     -> "Address change:" <+> pretty req
    BalanceTxReq utx         -> "Balance tx:" <+> pretty utx
    WriteBalancedTxReq tx    -> "Write balanced tx:" <+> pretty tx
    ExposeEndpointReq ep     -> "Expose endpoint:" <+> pretty ep

-- | Responses that 'Contract's receive
data PABResp =
    GetSlotConfigResp SlotConfig
    | AwaitSlotResp Slot
    | AwaitTimeResp POSIXTime
    | CurrentSlotResp Slot
    | CurrentTimeResp POSIXTime
    | AwaitTxConfirmedResp TxId
    | OwnContractInstanceIdResp ContractInstanceId
    | OwnPublicKeyResp PubKey
    | UtxoAtResp UtxoAtAddress
    | AddressChangeResp AddressChangeResponse
    | BalanceTxResp BalanceTxResponse
    | WriteBalancedTxResp WriteBalancedTxResponse
    | ExposeEndpointResp EndpointDescription (EndpointValue JSON.Value)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


instance Pretty PABResp where
  pretty = \case
    GetSlotConfigResp s         -> "Slot config:" <+> pretty s
    AwaitSlotResp s             -> "Slot:" <+> pretty s
    AwaitTimeResp s             -> "Time:" <+> pretty s
    CurrentSlotResp s           -> "Current slot:" <+> pretty s
    CurrentTimeResp s           -> "Current time:" <+> pretty s
    AwaitTxConfirmedResp txid   -> "Tx confirmed:" <+> pretty txid
    OwnContractInstanceIdResp i -> "Own contract instance ID:" <+> pretty i
    OwnPublicKeyResp k          -> "Own public key:" <+> pretty k
    UtxoAtResp rsp              -> "Utxo at:" <+> pretty rsp
    AddressChangeResp rsp       -> "Address change:" <+> pretty rsp
    BalanceTxResp r             -> "Balance tx:" <+> pretty r
    WriteBalancedTxResp r       -> "Write balanced tx:" <+> pretty r
    ExposeEndpointResp desc rsp -> "Call endpoint" <+> pretty desc <+> "with" <+> pretty rsp

matches :: PABReq -> PABResp -> Bool
matches a b = case (a, b) of
  (AwaitSlotReq{}, AwaitSlotResp{})                       -> True
  (AwaitTimeReq{}, AwaitTimeResp{})                       -> True
  (CurrentSlotReq, CurrentSlotResp{})                     -> True
  (CurrentTimeReq, CurrentTimeResp{})                     -> True
  (AwaitTxConfirmedReq{}, AwaitTxConfirmedResp{})         -> True
  (OwnContractInstanceIdReq, OwnContractInstanceIdResp{}) -> True
  (OwnPublicKeyReq, OwnPublicKeyResp{})                   -> True
  (UtxoAtReq{}, UtxoAtResp{})                             -> True
  (AddressChangeReq{}, AddressChangeResp{})               -> True
  (BalanceTxReq{}, BalanceTxResp{})                       -> True
  (WriteBalancedTxReq{}, WriteBalancedTxResp{})           -> True
  (ExposeEndpointReq ActiveEndpoint{aeDescription}, ExposeEndpointResp desc _)
    | aeDescription == desc -> True
  _                                                       -> False

data UtxoAtAddress =
    UtxoAtAddress
        { address :: Address
        , utxo    :: UtxoMap
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty UtxoAtAddress where
  pretty UtxoAtAddress{address, utxo} =
    let
      prettyTxOutPair (txoutref, TxOutTx{txOutTxOut}) =
        pretty txoutref <> colon <+> pretty txOutTxOut
      utxos = vsep $ fmap prettyTxOutPair (Map.toList utxo)
    in vsep ["Utxo at" <+> pretty address <+> "=", indent 2 utxos]

data BalanceTxResponse =
  BalanceTxFailed WalletAPIError
  | BalanceTxSuccess Tx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty BalanceTxResponse where
  pretty = \case
    BalanceTxFailed e  -> "BalanceTxFailed:" <+> pretty e
    BalanceTxSuccess i -> "BalanceTxSuccess:" <+> pretty (txId i)

balanceTxResponse :: Iso' BalanceTxResponse (Either WalletAPIError Tx)
balanceTxResponse = iso f g where
  f = \case { BalanceTxFailed w -> Left w; BalanceTxSuccess t -> Right t }
  g = either BalanceTxFailed BalanceTxSuccess

data WriteBalancedTxResponse =
  WriteBalancedTxFailed WalletAPIError
  | WriteBalancedTxSuccess Tx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty WriteBalancedTxResponse where
  pretty = \case
    WriteBalancedTxFailed e  -> "WriteBalancedTxFailed:" <+> pretty e
    WriteBalancedTxSuccess i -> "WriteBalancedTxSuccess:" <+> pretty (txId i)

writeBalancedTxResponse :: Iso' WriteBalancedTxResponse (Either WalletAPIError Tx)
writeBalancedTxResponse = iso f g where
  f = \case { WriteBalancedTxFailed w -> Left w; WriteBalancedTxSuccess t -> Right t }
  g = either WriteBalancedTxFailed WriteBalancedTxSuccess

data ActiveEndpoint = ActiveEndpoint
  { aeDescription :: EndpointDescription -- ^ The name of the endpoint
  , aeMetadata    :: Maybe JSON.Value -- ^ Data that should be shown to the user
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty ActiveEndpoint where
  pretty ActiveEndpoint{aeDescription, aeMetadata} =
    indent 2 $ vsep
      [ "Endpoint:" <+> pretty aeDescription
      , "Metadata:" <+> viaShow aeMetadata
      ]

newtype TxConfirmed =
    TxConfirmed { unTxConfirmed :: TxId }
        deriving stock (Eq, Ord, Generic, Show)
        deriving anyclass (ToJSON, FromJSON)
        deriving Pretty via TxId

makePrisms ''PABReq

makePrisms ''PABResp
