{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Logger     (MonadLogger, logInfoN, runStderrLoggingT)
import qualified Data.Text                as Text
import           Data.Time.Units          (Second, fromMicroseconds)
import           Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import           Options.Applicative      (CommandFields, Mod, Parser, auto, command, customExecParser, disambiguate,
                                           fullDesc, help, helper, idm, info, long, option, prefs, short, showDefault,
                                           showHelpOnEmpty, showHelpOnError, strOption, subparser, value)
import qualified Webserver

data Command = Webserver
  { _host       :: !HostPreference,
    _port       :: !Int,
    _timeoutSec :: !Second
  }
  deriving (Show, Eq)


commandParser :: Parser Command
commandParser = subparser webserverCommandParser

webserverCommandParser :: Mod CommandFields Command
webserverCommandParser =
  command "webserver"
    $ flip info fullDesc
    $ do
      _host <-
        strOption
          ( short 'b' <> long "bind" <> help "Webserver bind address"
              <> showDefault
              <> value "127.0.0.1"
          )
      _port <-
        option
          auto
          ( short 'p' <> long "port" <> help "Webserver port number"
              <> showDefault
              <> value 8888
          )
      _timeoutSec <-
          fromMicroseconds . (*1000000) <$> option
          auto
          ( short 't' <> long "timeout" <> help "timeout in seconds for interpretation"
              <> showDefault
              <> value 80
          )

      pure Webserver {..}

runCommand :: (MonadIO m, MonadLogger m) => Command -> m ()
runCommand Webserver {..} = Webserver.run settings _timeoutSec
  where
    settings = setHost _host . setPort _port $ defaultSettings

main :: IO ()
main = do
  options <-
    customExecParser
      (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
      (info (helper <*> commandParser) idm)
  runStderrLoggingT $ do
    logInfoN $ "Running: " <> Text.pack (show options)
    runCommand options
