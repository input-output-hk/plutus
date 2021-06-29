{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.PAB.Run.PSGenerator
    ( HasPSTypes(..)
    , generateDefault
    , generateWith
    , generateAPIModule
    , pabBridge
    , pabTypes
    ) where

import           Cardano.Metadata.Types                     (AnnotatedSignature, HashFunction, Property, PropertyKey,
                                                             Subject, SubjectProperties)
import           Cardano.Wallet.Types                       (WalletInfo)
import           Control.Applicative                        ((<|>))
import           Control.Lens                               (set, view, (&))
import           Control.Monad.Freer.Extras.Log             (LogLevel, LogMessage)
import           Data.Proxy                                 (Proxy (Proxy))
import qualified Data.Text                                  as Text
import           Data.Typeable                              (Typeable)
import           Language.PureScript.Bridge                 (BridgePart, Language (Haskell), SumType,
                                                             TypeInfo (TypeInfo), buildBridge, equal, genericShow,
                                                             haskType, mkSumType, order, typeModule, typeName,
                                                             writePSTypesWith, (^==))
import           Language.PureScript.Bridge.CodeGenSwitches (ForeignOptions (ForeignOptions), genForeign,
                                                             unwrapSingleConstructors)
import           Language.PureScript.Bridge.TypeParameters  (A)
import qualified PSGenerator.Common
import           Plutus.Contract.Checkpoint                 (CheckpointKey, CheckpointStore, CheckpointStoreItem)
import           Plutus.Contract.Effects                    (TxConfirmed)
import           Plutus.Contract.Resumable                  (Responses)
import           Plutus.PAB.Effects.Contract.Builtin        (Builtin)
import           Plutus.PAB.Events.ContractInstanceState    (PartiallyDecodedResponse)
import qualified Plutus.PAB.Webserver.API                   as API
import           Plutus.PAB.Webserver.Types                 (ChainReport, CombinedWSStreamToClient,
                                                             CombinedWSStreamToServer, ContractActivationArgs,
                                                             ContractInstanceClientState, ContractReport,
                                                             ContractSignatureResponse, FullReport,
                                                             InstanceStatusToClient)
import           Servant                                    ((:<|>))
import           Servant.PureScript                         (HasBridge, Settings, _generateSubscriberAPI, apiModuleName,
                                                             defaultBridge, defaultSettings, languageBridge,
                                                             writeAPIModuleWithSettings)

-- | List of types linked to contract type `a` that need to be available in
-- Purescript.
class HasPSTypes a where
    psTypes :: Proxy a -> [SumType 'Haskell]
    psTypes _ = []

-- | PAB's main bridge that includes common bridges
pabBridge :: BridgePart
pabBridge =
    PSGenerator.Common.aesonBridge <|>
    PSGenerator.Common.containersBridge <|>
    PSGenerator.Common.languageBridge <|>
    PSGenerator.Common.ledgerBridge <|>
    PSGenerator.Common.servantBridge <|>
    PSGenerator.Common.miscBridge <|>
    metadataBridge <|>
    defaultBridge

-- Some of the metadata types have a datakind type parameter that
-- PureScript won't support, so we must drop it.
metadataBridge :: BridgePart
metadataBridge = do
  (typeName ^== "Property")
    <|> (typeName ^== "SubjectProperties")
    <|> (typeName ^== "AnnotatedSignature")
  typeModule ^== "Cardano.Metadata.Types"
  moduleName <- view (haskType . typeModule)
  name <- view (haskType . typeName)
  pure $ TypeInfo "plutus-pab" moduleName name []

data PabBridge

pabBridgeProxy :: Proxy PabBridge
pabBridgeProxy = Proxy

instance HasBridge PabBridge where
    languageBridge _ = buildBridge pabBridge

-- | PAB's list of types that includes common types.
pabTypes :: [SumType 'Haskell]
pabTypes =
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.playgroundTypes <>
    PSGenerator.Common.walletTypes <>
    [ (equal <*> (genericShow <*> mkSumType)) (Proxy @(Builtin A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(FullReport A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ChainReport)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ContractReport A))
    , (equal <*> (genericShow <*> mkSumType))
          (Proxy @(ContractSignatureResponse A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(PartiallyDecodedResponse A))

    -- Contract request / response types
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxConfirmed)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @CheckpointStore)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @CheckpointKey)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(CheckpointStoreItem A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(Responses A))

    -- Logging types
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(LogMessage A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @LogLevel)

    -- Metadata types
    , (order <*> (genericShow <*> mkSumType)) (Proxy @Subject)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(SubjectProperties A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(Property A))
    , (order <*> (genericShow <*> mkSumType)) (Proxy @PropertyKey)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @HashFunction)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(AnnotatedSignature A))

    -- Web API types
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ContractActivationArgs A))
    , (genericShow <*> mkSumType) (Proxy @(ContractInstanceClientState A))
    , (genericShow <*> mkSumType) (Proxy @InstanceStatusToClient)
    , (genericShow <*> mkSumType) (Proxy @CombinedWSStreamToClient)
    , (genericShow <*> mkSumType) (Proxy @CombinedWSStreamToServer)
    , (genericShow <*> mkSumType) (Proxy @WalletInfo)
    ]

mySettings :: Settings
mySettings =
    (defaultSettings & set apiModuleName "Plutus.PAB.Webserver")
        {_generateSubscriberAPI = False}

------------------------------------------------------------
-- | Use the Proxy for specifying `a` when generating PS functions for the
-- webserver using a specific Builtin (ex. generate 'Builtin Marlowe' instead
-- of 'Builtin a').
generateAPIModule
    :: forall a. Typeable a
    => Proxy a
    -> FilePath -- ^ Output directory of PS files
    -> IO ()
generateAPIModule _ outputDir = do
    writeAPIModuleWithSettings
        mySettings
        outputDir
        pabBridgeProxy
        (    Proxy @(API.API (Builtin a)
        :<|> API.NewAPI (Builtin a) Text.Text
        :<|> API.WalletProxy Text.Text)
        )

-- | Generate PS modules in 'outputDir' which includes common types for the PAB.
generateDefault
    :: FilePath -- ^ Output directory of PS files
    -> IO ()
generateDefault outputDir = do
    writePSTypesWith
        (genForeign (ForeignOptions {unwrapSingleConstructors = True}))
        outputDir
        (buildBridge pabBridge)
        pabTypes
    putStrLn $ "Done: " <> outputDir

-- | Generate PS modules in 'outputDir' for types specified with 'HasPSTypes'
-- typeclass.
generateWith
    :: HasPSTypes a
    => Proxy a  -- ^ Proxy for type `a` which defines the 'HasPSTypes' typeclass.
    -> FilePath -- ^ Output directory of PS files
    -> IO ()
generateWith proxy outputDir = do
    writePSTypesWith
        (genForeign (ForeignOptions {unwrapSingleConstructors = True}))
        outputDir
        (buildBridge pabBridge)
        (psTypes proxy)
    putStrLn $ "Done: " <> outputDir
