{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Plutus.PAB.Run.CommandParser (parseOptions, AppOpts(..)) where

import           Cardano.BM.Data.Severity (Severity (..))
import           Options.Applicative      (CommandFields, Mod, Parser, argument, auto, command, customExecParser,
                                           disambiguate, flag, fullDesc, help, helper, idm, info, long, metavar, option,
                                           prefs, progDesc, short, showHelpOnEmpty, showHelpOnError, str, subparser,
                                           value)
import           Wallet.Types             (ContractInstanceId (..))

import           Plutus.PAB.App           (StorageBackend (..))
import           Plutus.PAB.Run.Command

data AppOpts = AppOpts { minLogLevel    :: Maybe Severity
                       , logConfigPath  :: Maybe FilePath
                       , configPath     :: Maybe FilePath
                       , runEkgServer   :: Bool
                       , storageBackend :: StorageBackend
                       , cmd            :: ConfigCommand
                       }

parseOptions :: IO AppOpts
parseOptions = customExecParser
            (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
            (info (helper <*> commandLineParser) idm)

logLevelFlag :: Parser (Maybe Severity)
logLevelFlag =
    flag
        Nothing
        (Just Debug)
        (short 'v' <> long "verbose" <> help "Enable debugging output.")

ekgFlag :: Parser Bool
ekgFlag =
    flag
        False
        True
        (short 'e' <> long "ekg" <> help "Enable the EKG server")

inMemoryFlag :: Parser StorageBackend
inMemoryFlag =
    flag
        BeamSqliteBackend
        InMemoryBackend
        (short 'm' <> long "memory" <> help "Use the memory-backed backend. If false, the beam backend is used.")

commandLineParser :: Parser AppOpts
commandLineParser =
        AppOpts <$> logLevelFlag
                <*> logConfigFileParser
                <*> configFileParser
                <*> ekgFlag
                <*> inMemoryFlag
                <*> commandParser

configFileParser :: Parser (Maybe FilePath)
configFileParser =
    option
        (Just <$> str)
        (long "config" <>
         metavar "CONFIG_FILE" <>
         help "Config file location." <> value Nothing)

logConfigFileParser :: Parser (Maybe FilePath)
logConfigFileParser =
    option
        (Just <$> str)
        (long "log-config" <>
         metavar "LOG_CONFIG_FILE" <>
         help "Logging config file location." <> value Nothing)

commandParser :: Parser ConfigCommand
commandParser =
    subparser $
    mconcat
        [ allServersParser
        , clientServicesParser
        , mockWalletParser
        , pabWebserverParser
        , mockNodeParser
        , chainIndexParser
        , metadataParser
        , command
              "contracts"
              (info
                   (subparser
                        (mconcat
                             [ reportActiveContractsParser
                             , contractStateParser
                             , reportContractHistoryParser
                             , reportAvailableContractsParser
                             ]))
                   (fullDesc <> progDesc "Manage your smart contracts."))
        , psGeneratorCommandParser
        ]

psGeneratorCommandParser :: Mod CommandFields ConfigCommand
psGeneratorCommandParser =
    command "psapigenerator" $
    flip info (fullDesc <> progDesc "Generate the frontend's PureScript files for the webserver API.") $ do
        psApiGenOutputDir <-
            argument
                str
                (metavar "OUTPUT_DIR" <>
                 help "Output directory to write PureScript files to.")
        pure $ PSApiGenerator {psApiGenOutputDir}

mockNodeParser :: Mod CommandFields ConfigCommand
mockNodeParser =
    command "node-server" $
    flip info (fullDesc <> progDesc "Run a mock version of the Cardano node API server.") $ do
        withoutMockServer <- flag WithMockServer WithoutMockServer
                                  (long "without-mock-node")
        pure $ MockNode withoutMockServer

mockWalletParser :: Mod CommandFields ConfigCommand
mockWalletParser =
    command "wallet-server" $
    info
        (pure MockWallet)
        (fullDesc <>
         progDesc "Run a mock version of the Cardano wallet API server.")

chainIndexParser :: Mod CommandFields ConfigCommand
chainIndexParser =
    command "chain-index" $
    info (pure ChainIndex) (fullDesc <> progDesc "Run the chain index.")

metadataParser :: Mod CommandFields ConfigCommand
metadataParser =
    command "metadata-server" $
    info (pure Metadata) (fullDesc <> progDesc "Run the Cardano metadata API server.")

allServersParser :: Mod CommandFields ConfigCommand
allServersParser =
    command "all-servers" $
    flip info (fullDesc <> progDesc "Run all the mock servers needed.") $ do
        withoutMockServer <- flag WithMockServer WithoutMockServer
                                  (long "without-mock-node")
        pure  (ForkCommands
                   [ MockNode withoutMockServer
                   , ChainIndex
                   , Metadata
                   , MockWallet
                   , PABWebserver
                   ])

clientServicesParser :: Mod CommandFields ConfigCommand
clientServicesParser =
    command "client-services" $
    info
        (pure (ForkCommands
                    [ ChainIndex
                    , Metadata
                    , MockWallet
                    , PABWebserver
                    ]))
        (fullDesc <> progDesc "Run the client services (all services except the mock node).")

contractStateParser :: Mod CommandFields ConfigCommand
contractStateParser =
    command "state" $
    info
        (ContractState <$> contractIdParser)
        (fullDesc <> progDesc "Show the current state of a contract.")

contractIdParser :: Parser ContractInstanceId
contractIdParser = fmap ContractInstanceId $
    argument
        auto
        (help "ID of the contract. (See 'active-contracts' for a list.)")

reportAvailableContractsParser :: Mod CommandFields ConfigCommand
reportAvailableContractsParser =
    command "available" $
    info
        (pure ReportAvailableContracts)
        (fullDesc <> progDesc "Show all available contracts.")

reportActiveContractsParser :: Mod CommandFields ConfigCommand
reportActiveContractsParser =
    command "active" $
    info
        (pure ReportActiveContracts)
        (fullDesc <> progDesc "Show all active contracts.")

pabWebserverParser :: Mod CommandFields ConfigCommand
pabWebserverParser =
    command "webserver" $
    info
        (pure PABWebserver)
        (fullDesc <> progDesc "Start the PAB backend webserver.")

reportContractHistoryParser :: Mod CommandFields ConfigCommand
reportContractHistoryParser =
    command "history" $
    info
        (ReportContractHistory <$> contractIdParser)
        (fullDesc <> progDesc "Show the state history of a smart contract.")

