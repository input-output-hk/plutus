{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Defines a number of types that are used in Wallet.XXX modules
module Wallet.Types(
    ContractInstanceId(..)
    , contractInstanceIDs
    , randomID
    , Notification(..)
    , EndpointDescription(..)
    , EndpointValue(..)
    , AddressChangeRequest(..)
    , targetSlot
    , slotRange
    , AddressChangeResponse(..)
    -- * Error types
    , MatchingError(..)
    , AsMatchingError(..)
    , AssertionError(..)
    , AsAssertionError(..)
    , ContractError(..)
    , AsContractError(..)
    , NotificationError(..)
    , AsNotificationError(..)
    ) where

import           Control.Lens                     (prism')
import           Control.Lens.TH                  (makeClassyPrisms)
import           Data.Aeson                       (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Encode.Pretty         as JSON
import qualified Data.ByteString.Lazy.Char8       as BSL8
import           Data.String                      (IsString (..))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc        (Pretty (..), colon, hang, viaShow, vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..), Tagged (..))
import           Data.UUID                        (UUID)
import qualified Data.UUID.Extras                 as UUID
import qualified Data.UUID.V4                     as UUID
import           GHC.Generics                     (Generic)
import qualified Language.Haskell.TH.Syntax       as TH
import           Servant.API

import           Ledger                           (Address, OnChainTx, Slot, SlotRange, eitherTx, interval, txId)
import           Ledger.Constraints.OffChain      (MkTxError)
import           Plutus.Contract.Checkpoint       (AsCheckpointError (..), CheckpointError)
import           Wallet.Emulator.Error            (WalletAPIError)


-- | An error
newtype MatchingError = WrongVariantError { unWrongVariantError :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
makeClassyPrisms ''MatchingError
instance Pretty MatchingError where
  pretty = \case
    WrongVariantError t -> "Wrong variant:" <+> pretty t

-- | An error emitted when an 'Assertion' fails.
newtype AssertionError = GenericAssertion { unAssertionError :: T.Text }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
makeClassyPrisms ''AssertionError

instance Pretty AssertionError where
    pretty = \case
        GenericAssertion t -> "Generic assertion:" <+> pretty t

-- | This lets people use 'T.Text' as their error type.
instance AsAssertionError T.Text where
    _AssertionError = prism' (T.pack . show) (const Nothing)

data ContractError =
    WalletError WalletAPIError
    | EmulatorAssertionError AssertionError -- TODO: Why do we need this constructor
    | OtherError T.Text
    | ConstraintResolutionError MkTxError
    | ResumableError MatchingError
    | CCheckpointError CheckpointError
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
makeClassyPrisms ''ContractError

instance Pretty ContractError where
  pretty = \case
    WalletError e               -> "Wallet error:" <+> pretty e
    EmulatorAssertionError a    -> "Emulator assertion error:" <+> pretty a
    OtherError t                -> "Other error:" <+> pretty t
    ConstraintResolutionError e -> "Constraint resolution error:" <+> pretty e
    ResumableError e            -> "Resumable error:" <+> pretty e
    CCheckpointError e          -> "Checkpoint error:" <+> pretty e

-- | This lets people use 'T.Text' as their error type.
instance AsContractError T.Text where
    _ContractError = prism' (T.pack . show) (const Nothing)

instance IsString ContractError where
  fromString = OtherError . fromString

instance AsAssertionError ContractError where
    _AssertionError = _EmulatorAssertionError

instance AsCheckpointError ContractError where
  _CheckpointError = _CCheckpointError

-- | Unique ID for contract instance
newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (FromJSONKey, ToJSONKey, ToHttpApiData, FromHttpApiData)
    deriving anyclass (FromJSON, ToJSON)
    deriving Pretty via (PrettyShow UUID)

-- | A pure list of all 'ContractInstanceId' values. To be used in testing.
contractInstanceIDs :: [ContractInstanceId]
contractInstanceIDs = ContractInstanceId <$> UUID.mockUUIDs

randomID :: IO ContractInstanceId
randomID = ContractInstanceId <$> UUID.nextRandom

newtype EndpointDescription = EndpointDescription { getEndpointDescription :: String }
    deriving stock (Eq, Ord, Generic, Show, TH.Lift)
    deriving newtype (IsString, Pretty)
    deriving anyclass (ToJSON, FromJSON)

newtype EndpointValue a = EndpointValue { unEndpointValue :: a }
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

deriving via (Tagged "EndpointValue:" (PrettyShow a)) instance (Show a => Pretty (EndpointValue a))

data Notification =
    Notification
        { notificationContractID       :: ContractInstanceId
        , notificationContractEndpoint :: EndpointDescription
        , notificationContractArg      :: Aeson.Value
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty Notification where
    pretty Notification{notificationContractID,notificationContractEndpoint,notificationContractArg} =
        hang 2 $ vsep
            [ "Instance:" <+> pretty notificationContractID
            , "Endpoint:" <+> pretty notificationContractEndpoint
            , "Argument:" <+> viaShow notificationContractArg
            ]

data NotificationError =
    EndpointNotAvailable ContractInstanceId EndpointDescription
    | MoreThanOneEndpointAvailable ContractInstanceId EndpointDescription
    | InstanceDoesNotExist ContractInstanceId
    | OtherNotificationError ContractError
    | NotificationJSONDecodeError EndpointDescription Aeson.Value String -- ^ Indicates that the target contract does not have the expected schema
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty NotificationError where
    pretty = \case
        EndpointNotAvailable i ep -> "Endpoint" <+> pretty ep <+> "not available on" <+> pretty i
        MoreThanOneEndpointAvailable i ep -> "Endpoint" <+> pretty ep <+> "is exposed more than once on" <+> pretty i
        InstanceDoesNotExist i -> "Instance does not exist:" <+> pretty i
        OtherNotificationError e -> "Other notification error:" <+> pretty e
        NotificationJSONDecodeError ep vv e ->
                "Notification JSON decoding error:"
                    <+> pretty e
                    <> colon
                    <+> pretty (BSL8.unpack (JSON.encodePretty vv))
                    <+> pretty ep

makeClassyPrisms ''NotificationError

-- | Information about transactions that spend or produce an output at
--   an address in a slot range.
data AddressChangeResponse =
    AddressChangeResponse
        { acrAddress   :: Address -- ^ The address
        , acrSlotRange :: SlotRange -- ^ The slot range
        , acrTxns      :: [OnChainTx] -- ^ Transactions that were validated in the slot range and spent or produced at least one output at the address.
        }
        deriving stock (Eq, Generic, Show)
        deriving anyclass (ToJSON, FromJSON)

instance Pretty AddressChangeResponse where
    pretty AddressChangeResponse{acrAddress, acrTxns, acrSlotRange} =
        hang 2 $ vsep
            [ "Address:" <+> pretty acrAddress
            , "Slot range:" <+> pretty acrSlotRange
            , "Tx IDs:" <+> pretty (eitherTx txId txId <$> acrTxns)
            ]

-- | Request for information about transactions that spend or produce
--   outputs at a specific address in a slot range.
data AddressChangeRequest =
    AddressChangeRequest
        { acreqSlotRangeFrom :: Slot
        , acreqSlotRangeTo   :: Slot
        , acreqAddress       :: Address -- ^ The address
        }
        deriving stock (Eq, Generic, Show, Ord)
        deriving anyclass (ToJSON, FromJSON)

instance Pretty AddressChangeRequest where
    pretty AddressChangeRequest{acreqSlotRangeFrom, acreqSlotRangeTo, acreqAddress} =
        hang 2 $ vsep
            [ "From " <+> pretty acreqSlotRangeFrom <+> "to" <+> pretty acreqSlotRangeTo
            , "Address:" <+> pretty acreqAddress
            ]

-- | The earliest slot in which we can respond to an 'AddressChangeRequest'.
targetSlot :: AddressChangeRequest -> Slot
targetSlot = succ . acreqSlotRangeTo

-- | The slot range for this request
slotRange :: AddressChangeRequest -> SlotRange
slotRange AddressChangeRequest{acreqSlotRangeFrom, acreqSlotRangeTo} =
    interval acreqSlotRangeFrom acreqSlotRangeTo
