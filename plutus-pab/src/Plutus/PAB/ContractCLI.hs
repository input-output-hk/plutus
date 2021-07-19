{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Create a command line application from a @Contract schema Text ()@. For example:

> game :: AsContractError e => Contract GameSchema e ()
> game = lock <|> guess
>
> ...
>
> main :: IO ()
> main = commandLineApp game

-}
module Plutus.PAB.ContractCLI
    ( commandLineApp
    , commandLineApp'
    , runCliCommand
    , runUpdate
    , Command(..)
    -- * Debugging
    , contractCliApp
    , runPromptPure
    , runPromptIO
    ) where

import           Control.Monad.Freer               (Eff, LastMember, Member, interpret, run, runM, send, sendM,
                                                    type (~>))
import           Control.Monad.Freer.Error         (Error, runError, throwError)
import qualified Control.Monad.Freer.Extras.Modify as Modify
import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson                        (FromJSON, ToJSON (toJSON))
import qualified Data.Aeson                        as JSON
import qualified Data.Aeson.Encode.Pretty          as JSON
import           Data.Bifunctor                    (bimap)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy              as BSL
import           Data.Foldable                     (traverse_)
import           Data.Proxy                        (Proxy (..))
import           Data.Row.Extras                   (type (.\\))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Options.Applicative               (CommandFields, Mod, Parser, ParserResult, command, disambiguate,
                                                    execParserPure, fullDesc, helper, idm, info, prefs, progDesc,
                                                    showHelpOnEmpty, showHelpOnError, subparser)
import qualified Options.Applicative
import           Playground.Schema                 (EndpointToSchema, endpointsToSchemas)
import           Plutus.Contract                   (Contract, EmptySchema)
import qualified Plutus.Contract.State             as ContractState
import           Prelude                           hiding (getContents)
import           System.Environment                (getArgs)
import           System.Exit                       (ExitCode (ExitFailure), exitSuccess, exitWith)
import qualified System.IO

-- | Read from stdin
data CliEff r where
    GetContents :: CliEff BS.ByteString -- ^ Read from stdin
    HandleParseResult :: Show a => ParserResult a -> CliEff a -- ^ Deal with a 'ParserResult' from @optparse-applicative@

getContents :: forall effs. Member CliEff effs => Eff effs BS.ByteString
getContents = send GetContents

handleParseResult :: forall a effs. (Member CliEff effs, Show a) => ParserResult a -> Eff effs a
handleParseResult = send . HandleParseResult

-- | A program that takes some argument, reads some input
--   and returns either an error or a response
type Prompt a = Eff '[CliEff, Error [BS.ByteString]] a

data Command
    = Initialise
    | Update
    | ExportSignature
    deriving (Show, Eq)

commandLineParser :: Parser Command
commandLineParser = subparser $ mconcat [initialiseParser, updateParser, exportSignatureParser]

initialiseParser :: Mod CommandFields Command
initialiseParser =
    command "init" $
    info (pure Initialise) (fullDesc <> progDesc "Initialise the contract.")

updateParser :: Mod CommandFields Command
updateParser =
    command "update" $
    info
        (pure Update)
        (fullDesc <>
         progDesc "Update the contract. The request must be supplied on stdin.")


exportSignatureParser :: Mod CommandFields Command
exportSignatureParser =
    command "export-signature" $
    info (pure ExportSignature) (fullDesc <> progDesc "Export the contract's signature.")

runCliCommand :: forall w s s2 ignored.
       ( EndpointToSchema (s .\\ s2)
       , ToJSON w
       , FromJSON w
       , Monoid w
       )
    => Proxy s2
    -> Contract w s Text ignored
    -> Command
    -> Prompt BS8.ByteString
runCliCommand _ schema Initialise = pure $ BSL.toStrict $ JSON.encodePretty $ ContractState.initialiseContract schema
runCliCommand _ schema Update = do
    arg <- getContents
    runUpdate schema arg
runCliCommand _ _ ExportSignature = do
  let r = endpointsToSchemas @(s .\\ s2)
  pure $ BSL.toStrict $ JSON.encodePretty r

runUpdate :: forall w s ignored.
    ( ToJSON w
    , FromJSON w
    , Monoid w
    )
    => Contract w s Text ignored
    -> BS.ByteString
    -> Prompt BS8.ByteString
runUpdate contract arg = either (throwError @[BS.ByteString] . return) pure $
    bimap
        (BSL.toStrict . JSON.encodePretty . Text.pack)
        (BSL.toStrict . JSON.encodePretty . ContractState.mapE toJSON . ContractState.mapW toJSON . bimap toJSON toJSON . ContractState.insertAndUpdateContract contract)
        (JSON.eitherDecode $ BSL.fromStrict arg)

-- | Make a command line app with a schema that includes all of the contract's
--   endpoints
commandLineApp :: forall w s ignored.
       ( EndpointToSchema (s .\\ EmptySchema)
       , ToJSON w
       , FromJSON w
       , Monoid w
       )
    => Contract w s Text ignored
    -> IO ()
commandLineApp = commandLineApp' @w @s @EmptySchema (Proxy @EmptySchema)

-- | Make a command line app for a contract, excluding some of the contract's
--   endpoints from the generated schema.
commandLineApp' :: forall w s s2 ignored.
       ( EndpointToSchema (s .\\ s2)
       , ToJSON w
       , FromJSON w
       , Monoid w
       )
    => Proxy s2
    -> Contract w s Text ignored
    -> IO ()
commandLineApp' p schema = runPromptIO (contractCliApp p schema)

contractCliApp :: forall w s s2 ignored.
       ( EndpointToSchema (s .\\ s2)
       , ToJSON w
       , FromJSON w
       , Monoid w
       )
    => Proxy s2
    -> Contract w s Text ignored
    -> [String]
    -> Prompt BS.ByteString
contractCliApp p schema args = do
    cmd <- handleParseResult $ execParserPure (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError) (info (helper <*> commandLineParser) idm) args
    runCliCommand p schema cmd

handleCliEffIO ::
    forall effs.
    LastMember IO effs
    => CliEff
    ~> Eff effs
handleCliEffIO = \case
    GetContents         -> sendM BS.getContents
    HandleParseResult a -> liftIO (Options.Applicative.handleParseResult a)

handleCliEffPure ::
    forall effs.
    Member (Error [BS.ByteString]) effs
    => BS.ByteString
    -> CliEff
    ~> Eff effs
handleCliEffPure input = \case
    GetContents -> return input
    HandleParseResult a -> case Options.Applicative.getParseResult a of
        Nothing -> throwError @[BS.ByteString] [BS8.pack $ show a]
        Just a' -> pure a'

runPromptIO :: ([String] -> Prompt BS.ByteString) -> IO ()
runPromptIO p = do
    args <- getArgs
    result <- runM $ runError $ interpret handleCliEffIO $ Modify.raiseEnd $ p args
    case result of
        Left errs -> do
            traverse_ (BS8.hPut System.IO.stderr) errs
            exitWith $ ExitFailure 1
        Right response -> do
            BS8.putStrLn response
            exitSuccess

runPromptPure :: Prompt a -> BS.ByteString -> Either [BS.ByteString] a
runPromptPure p input = run $ runError $ interpret (handleCliEffPure input) p
