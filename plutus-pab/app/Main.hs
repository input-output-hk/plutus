{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main
    ( main
    ) where

import           CommandParser
import           Plutus.PAB.Run.Cli

import qualified Cardano.BM.Configuration.Model      as CM
import           Cardano.BM.Data.Trace               (Trace)
import           Cardano.BM.Setup                    (setupTrace_)
import           Control.Monad.Logger                (logErrorN, runStdoutLoggingT)
import           Data.Foldable                       (for_)
import           Data.Text.Extras                    (tshow)
import           Data.Void                           (Void)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import           Plutus.PAB.Monitoring.Config        (defaultConfig, loadConfig)
import           Plutus.PAB.Monitoring.PABLogMsg     (AppMsg (..))
import           Plutus.PAB.Monitoring.Util          (PrettyObject (..), convertLog)
import           Plutus.PAB.Types                    (PABError)
import           System.Exit                         (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = do
    AppOpts { minLogLevel, logConfigPath, cmd } <- parseOptions

    -- Parse config files and initialize logging
    logConfig <- maybe defaultConfig loadConfig logConfigPath
    for_ minLogLevel $ \ll -> CM.setMinSeverity logConfig ll
    (trace :: Trace IO (PrettyObject (AppMsg (Builtin Void))), _) <- setupTrace_ logConfig "pab"

    -- execute parsed pab command and handle errors on failure
    result <- Right <$> runNoConfigCommand (convertLog PrettyObject trace) cmd
    either handleError (const exitSuccess) result

    where
        handleError (err :: PABError) = do
            runStdoutLoggingT $ (logErrorN . tshow) err
            exitWith (ExitFailure 1)
