{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Plutus.PAB.Run
( runWith
) where

import qualified Cardano.BM.Backend.EKGView          as EKGView
import qualified Cardano.BM.Configuration.Model      as CM
import           Cardano.BM.Data.Trace               (Trace)
import           Cardano.BM.Plugin                   (loadPlugin)
import           Cardano.BM.Setup                    (setupTrace_)
import           Control.Concurrent.Availability     (newToken)
import           Control.Monad                       (when)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Logger                (logErrorN, runStdoutLoggingT)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Foldable                       (for_)
import           Data.Text.Extras                    (tshow)
import           Data.Typeable                       (Typeable)
import           Data.Yaml                           (decodeFileThrow)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions)
import           Plutus.PAB.Monitoring.Config        (defaultConfig, loadConfig)
import           Plutus.PAB.Monitoring.PABLogMsg     (AppMsg (..))
import           Plutus.PAB.Monitoring.Util          (PrettyObject (..), convertLog)
import           Plutus.PAB.Run.Cli
import           Plutus.PAB.Run.CommandParser
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes)
import           Plutus.PAB.Types                    (PABError (MissingConfigFileOption))
import           Prettyprinter                       (Pretty)
import qualified Servant
import           System.Exit                         (ExitCode (ExitFailure), exitSuccess, exitWith)

-- | PAB entry point for a contract type `a`.
runWith :: forall a.
    ( Show a
    , Ord a
    , FromJSON a
    , ToJSON a
    , Pretty a
    , Servant.MimeUnrender Servant.JSON a
    , Typeable a
    , HasDefinitions a
    , HasPSTypes a
    )
    => BuiltinHandler a -- ^ Builtin contract handler. Can be created with 'Plutus.PAB.Effects.Contract.Builtin.handleBuiltin'.
    -> IO ()
runWith userContractHandler = do
    AppOpts { minLogLevel, logConfigPath, runEkgServer, cmd, configPath, storageBackend } <- parseOptions

    -- Parse config files and initialize logging
    logConfig <- maybe defaultConfig loadConfig logConfigPath
    for_ minLogLevel $ \ll -> CM.setMinSeverity logConfig ll
    (trace :: Trace IO (PrettyObject (AppMsg (Builtin a))), switchboard) <- setupTrace_ logConfig "pab"

    -- enable EKG backend
    when runEkgServer $ EKGView.plugin logConfig trace switchboard >>= loadPlugin switchboard

    -- obtain token for signaling service readiness
    serviceAvailability <- newToken

    -- execute parsed pab command and handle errors on faliure
    result <- case configPath of
        Nothing -> pure $ Left MissingConfigFileOption
        Just p -> do
            config <- liftIO $ decodeFileThrow p
            let args = ConfigCommandArgs
                        { ccaTrace = convertLog PrettyObject trace
                        , ccaLoggingConfig = logConfig
                        , ccaPABConfig = config
                        , ccaAvailability = serviceAvailability
                        , ccaStorageBackend = storageBackend
                        }
            Right <$> runConfigCommand userContractHandler args cmd
    either handleError (const exitSuccess) result

        where

            handleError (err :: PABError) = do
                runStdoutLoggingT $ (logErrorN . tshow) err
                exitWith (ExitFailure 1)
