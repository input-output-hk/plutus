{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | PAB Log messages and instances
module Plutus.PAB.Monitoring.PABLogMsg(
    PABLogMsg(..),
    ContractExeLogMsg(..),
    ChainIndexServerMsg,
    MetadataLogMessage,
    WalletMsg,
    MockServerLogMsg,
    AppMsg(..),
    CoreMsg(..),
    PABMultiAgentMsg(..),
    ContractEffectMsg(..),
    RequestSize(..)
    ) where

import           Data.Aeson                              (FromJSON, ToJSON, Value)
import qualified Data.Aeson                              as JSON
import           Data.String                             (IsString (..))
import           Data.Text                               (Text)
import           Data.Text.Prettyprint.Doc               (Pretty (..), colon, hang, parens, viaShow, vsep, (<+>))
import           GHC.Generics                            (Generic)

import           Cardano.BM.Data.Tracer                  (ToObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Tracer.Extras           (StructuredLog, Tagged (..), mkObjectStr)
import           Cardano.ChainIndex.Types                (ChainIndexServerMsg)
import           Cardano.Metadata.Types                  (MetadataLogMessage)
import           Cardano.Node.Types                      (MockServerLogMsg)
import           Cardano.Wallet.Types                    (WalletMsg)
import           Data.Aeson.Text                         (encodeToLazyText)
import qualified Data.Text                               as T
import           Plutus.Contract.Effects                 (PABReq)
import           Plutus.Contract.Resumable               (Response)
import           Plutus.Contract.State                   (ContractRequest, ContractResponse)
import           Plutus.PAB.Core.ContractInstance        (ContractInstanceMsg (..))
import           Plutus.PAB.Effects.Contract             (PABContract (..))
import           Plutus.PAB.Effects.ContractRuntime      (ContractRuntimeMsg)
import           Plutus.PAB.Events.Contract              (ContractInstanceId)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import           Plutus.PAB.Instances                    ()
import           Plutus.PAB.ParseStringifiedJSON         (UnStringifyJSONLog (..))
import           Plutus.PAB.Types                        (PABError)
import           Wallet.Emulator.LogMessages             (TxBalanceMsg)
import           Wallet.Emulator.MultiAgent              (EmulatorEvent)
import           Wallet.Emulator.Wallet                  (Wallet, WalletEvent (..))

data AppMsg t =
    InstalledContractsMsg
    | ActiveContractsMsg
    | ContractHistoryMsg
    | PABMsg (PABLogMsg t)
    | InstalledContract Text
    | ContractInstances (ContractDef t) [ContractInstanceId]
    | ContractHistoryItem ContractInstanceId (Response JSON.Value)
    deriving stock (Generic)

deriving stock instance (Show (ContractDef t)) => Show (AppMsg t)
deriving anyclass instance (ToJSON (ContractDef t)) => ToJSON (AppMsg t)
deriving anyclass instance (FromJSON (ContractDef t)) => FromJSON (AppMsg t)

instance (Pretty (ContractDef t)) => Pretty (AppMsg t) where
    pretty = \case
        InstalledContractsMsg            -> "Installed contracts"
        ActiveContractsMsg               -> "Active contracts"
        ContractHistoryMsg               -> "Contract history"
        PABMsg m                         -> pretty m
        InstalledContract t              -> pretty t
        ContractInstances t s            -> pretty t <+> pretty s
        ContractHistoryItem instanceId s -> pretty instanceId <> colon <+> pretty (fmap encodeToLazyText s)

data PABLogMsg t =
    SContractExeLogMsg ContractExeLogMsg
    | SContractInstanceMsg (ContractInstanceMsg t)
    | SCoreMsg (CoreMsg t)
    | SUnstringifyJSON UnStringifyJSONLog
    | SWalletEvent Wallet.Emulator.Wallet.WalletEvent
    | SContractRuntimeMsg ContractRuntimeMsg
    | SChainIndexServerMsg ChainIndexServerMsg
    | SWalletMsg WalletMsg
    | SMetaDataLogMsg MetadataLogMessage
    | SMockserverLogMsg MockServerLogMsg
    | SMultiAgent (PABMultiAgentMsg t)
    deriving stock (Generic)

deriving stock instance (Show (ContractDef t)) => Show (PABLogMsg t)
deriving anyclass instance (ToJSON (ContractDef t)) => ToJSON (PABLogMsg t)
deriving anyclass instance (FromJSON (ContractDef t)) => FromJSON (PABLogMsg t)

instance Pretty (ContractDef t) => Pretty (PABLogMsg t) where
    pretty = \case
        SContractExeLogMsg m   -> pretty m
        SContractInstanceMsg m -> pretty m
        SCoreMsg m             -> pretty m
        SUnstringifyJSON m     -> pretty m
        SWalletEvent w         -> pretty w
        SContractRuntimeMsg m  -> pretty m
        SChainIndexServerMsg m -> pretty m
        SWalletMsg m           -> pretty m
        SMetaDataLogMsg m      -> pretty m
        SMockserverLogMsg m    -> pretty m
        SMultiAgent m          -> pretty m



{- ToObject instances

'ToObject.toObject' is very similar to 'ToJSON.toJSON' except that

* 'toObject' has an additional paramter for verbosity
* 'toObject' must always produce a JSON object (key-value map)

In the definitions below, every object produced by 'toObject' has a field
'string' with a friendly description of the message, similar to its
'Pretty' instance. Additional fields depend on the type of message.

-}

instance (ToJSON (ContractDef t), StructuredLog (ContractDef t)) => ToObject (AppMsg t) where
    toObject v = \case
        InstalledContractsMsg ->
            mkObjectStr "Listing installed contracts" ()
        ActiveContractsMsg ->
            mkObjectStr "Listing active contract instances" ()
        ContractHistoryMsg ->
            mkObjectStr "Showing contract history" ()
        PABMsg m -> toObject v m
        InstalledContract t ->
            mkObjectStr "Installed contract" t
        ContractInstances exe ids ->
            mkObjectStr
                "Active instances for contract"
                (exe, Tagged @"instances" ids)
        ContractHistoryItem i state ->
            mkObjectStr "Contract history item" $
                case v of
                    MaximalVerbosity -> Left (i, state)
                    _                -> Right i

instance (StructuredLog (ContractDef t), ToJSON (ContractDef t)) => ToObject (PABLogMsg t) where
    toObject v = \case
        SContractExeLogMsg m   -> toObject v m
        SContractInstanceMsg m -> toObject v m
        SCoreMsg m             -> toObject v m
        SUnstringifyJSON m     -> toObject v m
        SWalletEvent e         -> toObject v e
        SContractRuntimeMsg e  -> toObject v e
        SChainIndexServerMsg m -> toObject v m
        SWalletMsg m           -> toObject v m
        SMetaDataLogMsg m      -> toObject v m
        SMockserverLogMsg m    -> toObject v m
        SMultiAgent m          -> toObject v m

-- | FIXME: Redundant?
data PABMultiAgentMsg t =
    EmulatorMsg EmulatorEvent
    | ContractMsg ContractEffectMsg
    | MetadataLog MetadataLogMessage
    | ChainIndexServerLog ChainIndexServerMsg
    | ContractInstanceLog (ContractInstanceMsg t)
    | CoreLog (CoreMsg t)
    | RuntimeLog ContractRuntimeMsg
    | UserLog T.Text
    | SqlLog String
    | StartingPABBackendServer Int
    | StartingMetadataServer Int
    | WalletBalancingMsg Wallet TxBalanceMsg
    deriving stock Generic

instance (StructuredLog (ContractDef t), ToJSON (ContractDef t)) => ToObject (PABMultiAgentMsg t) where
    toObject v = \case
        EmulatorMsg e              -> mkObjectStr "emulator message" (Tagged @"payload" e)
        ContractMsg e              -> mkObjectStr "contract message" (Tagged @"payload" e)
        MetadataLog m              -> toObject v m
        ChainIndexServerLog m      -> toObject v m
        ContractInstanceLog m      -> toObject v m
        CoreLog m                  -> toObject v m
        RuntimeLog m               -> toObject v m
        UserLog t                  -> toObject v t
        SqlLog s                   -> toObject v s
        StartingPABBackendServer i -> mkObjectStr "starting backend server" (Tagged @"port" i)
        StartingMetadataServer i   -> mkObjectStr "starting backend server" (Tagged @"port" i)
        WalletBalancingMsg w m     -> mkObjectStr "balancing" (Tagged @"wallet" w, Tagged @"message" m)

deriving stock instance (Show (ContractDef t)) => Show (PABMultiAgentMsg t)
deriving anyclass instance (ToJSON (ContractDef t)) => ToJSON (PABMultiAgentMsg t)
deriving anyclass instance (FromJSON (ContractDef t)) => FromJSON (PABMultiAgentMsg t)

instance Pretty (ContractDef t) => Pretty (PABMultiAgentMsg t) where
    pretty = \case
        EmulatorMsg m         -> pretty m
        ContractMsg m         -> pretty m
        MetadataLog m         -> pretty m
        ChainIndexServerLog m -> pretty m
        ContractInstanceLog m -> pretty m
        CoreLog m             -> pretty m
        RuntimeLog m          -> pretty m
        UserLog m             -> pretty m
        SqlLog m              -> pretty m
        StartingPABBackendServer port ->
            "Starting PAB backend server on port:" <+> pretty port
        StartingMetadataServer port ->
            "Starting metadata server on port:" <+> pretty port
        WalletBalancingMsg w m -> pretty w <> colon <+> pretty m

data CoreMsg t =
    Installing (ContractDef t)
    | Installed
    | FindingContract ContractInstanceId
    | FoundContract (Maybe (ContractResponse Value Value Value PABReq))
    deriving stock Generic

deriving stock instance (Show (ContractDef t)) => Show (CoreMsg t)
deriving anyclass instance (ToJSON (ContractDef t)) => ToJSON (CoreMsg t)
deriving anyclass instance (FromJSON (ContractDef t)) => FromJSON (CoreMsg t)

instance Pretty (ContractDef t) => Pretty (CoreMsg t) where
    pretty = \case
        Installing d      -> "Installing" <+> pretty d
        Installed         -> "Installed"
        FindingContract i -> "Finding contract" <+> pretty i
        FoundContract c   -> "Found contract" <+> viaShow c

instance (StructuredLog (ContractDef t), ToJSON (ContractDef t)) => ToObject (CoreMsg t) where
    toObject v = \case
        Installing t ->
            mkObjectStr "installing contract" t
        Installed ->
            mkObjectStr "contract installed" ()
        FindingContract instanceID ->
            mkObjectStr "finding contract instance" instanceID
        FoundContract state ->
            mkObjectStr "found contract" $
                case v of
                    MaximalVerbosity -> Left (Tagged @"contract" state)
                    _                -> Right ()

data ContractEffectMsg =
    SendContractRequest (ContractRequest JSON.Value JSON.Value)
    | ReceiveContractResponse (PartiallyDecodedResponse PABReq)
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty ContractEffectMsg where
    pretty = \case
        SendContractRequest vl      -> "Request:" <+> pretty vl
        ReceiveContractResponse rsp -> "Response:" <+> pretty rsp

newtype RequestSize = RequestSize Int
    deriving stock (Show)
    deriving newtype (ToJSON, FromJSON)

instance Pretty RequestSize where
    pretty (RequestSize i) = pretty i <+> "bytes"

data ContractExeLogMsg =
    InvokeContractMsg
    | InitContractMsg FilePath
    | UpdateContractMsg FilePath RequestSize String
    | ExportSignatureMsg FilePath
    | ProcessExitFailure String
    | ContractResponseJSONDecodingError String
    | AContractResponse String
    | Migrating
    | InvokingEndpoint String JSON.Value
    | EndpointInvocationResponse [Text]
    | ContractExePABError PABError
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty ContractExeLogMsg where
    pretty = \case
        InvokeContractMsg -> "InvokeContract"
        InitContractMsg fp -> fromString fp <+> "init"
        UpdateContractMsg fp vl _ -> "updating" <+> fromString fp <+> parens (pretty vl)
        ExportSignatureMsg fp -> fromString fp <+> "export-signature"
        ProcessExitFailure err -> "ExitFailure" <+> pretty err
        ContractResponseJSONDecodingError err -> "Error decoding contract response from JSON:" <+> pretty err
        AContractResponse str -> pretty str
        Migrating -> "Migrating"
        InvokingEndpoint s v ->
            "Invoking:" <+> pretty s <+> "/" <+> viaShow v
        EndpointInvocationResponse v ->
            hang 2 $ vsep ("Invocation response:" : fmap pretty v)
        ContractExePABError e ->
            "PAB error:" <+> pretty e

instance ToObject ContractExeLogMsg where
    toObject v = \case
        InvokeContractMsg -> mkObjectStr "invoking contract" ()
        InitContractMsg fp ->
            mkObjectStr "Initialising contract" (Tagged @"file_path" fp)
        UpdateContractMsg fp (RequestSize rq) msg ->
            let f = Tagged @"file_path" fp
                g = Tagged @"request_size" rq
                c = Tagged @"content" msg
            in mkObjectStr "updating contract" (f, g, c)
        ExportSignatureMsg fp ->
            mkObjectStr "exporting signature" (Tagged @"file_path" fp)
        ProcessExitFailure f ->
            mkObjectStr "process exit failure" (Tagged @"error" f)
        ContractResponseJSONDecodingError e ->
            mkObjectStr "contract response JSON decoding error" (Tagged @"error" e)
        AContractResponse r ->
            mkObjectStr "received contract response" $
                case v of
                    MaximalVerbosity -> Left (Tagged @"response" r)
                    _                -> Right ()
        Migrating -> mkObjectStr "migrating database" ()
        InvokingEndpoint ep vl ->
            mkObjectStr "Invoking endpoint" $
                case v of
                    MinimalVerbosity -> Left (Tagged @"endpoint" ep)
                    _                -> Right (Tagged @"endpoint" ep, Tagged @"argument" vl)
        EndpointInvocationResponse lns ->
            mkObjectStr "endpoint invocation response"  (Tagged @"reponse" lns)
        ContractExePABError err ->
            mkObjectStr "contract executable error" (Tagged @"error" err)

