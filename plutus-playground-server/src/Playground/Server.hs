{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Playground.Server where

import qualified Auth
import           Auth.Types                   (OAuthClientId (OAuthClientId), OAuthClientSecret (OAuthClientSecret))
import           Control.Monad.Except         (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Logger         (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader         (ReaderT, runReaderT)
import           Data.Aeson                   (decodeFileStrict)
import           Data.Bits                    (toIntegralSized)
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time.Units              (Second, toMicroseconds)
import           Language.Haskell.Interpreter (InterpreterError (CompilationErrors), InterpreterResult, SourceCode)
import qualified Language.Haskell.Interpreter as Interpreter
import           Network.HTTP.Client.Conduit  (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)
import           Network.HTTP.Conduit         (newManager)
import           Network.Wai.Middleware.Cors  (cors, simpleCorsResourcePolicy)
import qualified Playground.Interpreter       as PI
import           Playground.Types             (CompilationResult, Evaluation, EvaluationResult, PlaygroundError)
import           Playground.Usecases          (vesting)
import           Servant                      (Application, err400, errBody, hoistServer, serve)
import           Servant.API                  (Get, JSON, Post, ReqBody, (:<|>) ((:<|>)), (:>))
import           Servant.Client               (ClientEnv, mkClientEnv, parseBaseUrl)
import           Servant.Server               (Handler (Handler), Server, ServerError)
import           System.Environment           (lookupEnv)
import qualified Web.JWT                      as JWT

type API
     = "contract" :> ReqBody '[ JSON] SourceCode :> Post '[ JSON] (Either Interpreter.InterpreterError (InterpreterResult CompilationResult))
       :<|> "evaluate" :> ReqBody '[ JSON] Evaluation :> Post '[ JSON] (Either PlaygroundError EvaluationResult)
       :<|> "health" :> Get '[ JSON] ()

type Web = "api" :> (API :<|> Auth.API)

compileSourceCode ::
       ClientEnv
    -> SourceCode
    -> Handler (Either InterpreterError (InterpreterResult CompilationResult))
compileSourceCode clientEnv sourceCode = do
    r <- liftIO . runExceptT $ PI.compile clientEnv sourceCode
    case r of
        Right vs -> pure . Right $ vs
        Left (CompilationErrors errors) ->
            pure . Left $ CompilationErrors errors
        Left e -> throwError $ err400 {errBody = BSL.pack . show $ e}

evaluateSimulation ::
       ClientEnv -> Evaluation -> Handler (Either PlaygroundError EvaluationResult)
evaluateSimulation clientEnv evaluation = do
    result <-
        liftIO . runExceptT $
        PI.evaluateSimulation clientEnv evaluation
    pure $ Interpreter.result <$> result

checkHealth :: ClientEnv -> Handler ()
checkHealth clientEnv =
    compileSourceCode clientEnv vesting >>= \case
        Left e  -> throwError $ err400 {errBody = BSL.pack . show $ e}
        Right _ -> pure ()

liftedAuthServer :: Auth.GithubEndpoints -> Auth.Config -> Server Auth.API
liftedAuthServer githubEndpoints config =
  hoistServer (Proxy @Auth.API) liftAuthToHandler Auth.server
  where
    liftAuthToHandler ::
      ReaderT (Auth.GithubEndpoints, Auth.Config) (LoggingT (ExceptT ServerError IO)) a ->
      Handler a
    liftAuthToHandler =
      Handler . runStderrLoggingT . flip runReaderT (githubEndpoints, config)

mkHandlers :: MonadIO m => AppConfig -> m (Server Web)
mkHandlers AppConfig {..} = do
    liftIO $ putStrLn "Interpreter ready"
    githubEndpoints <- liftIO Auth.mkGithubEndpoints
    pure $ (compileSourceCode clientEnv :<|> evaluateSimulation clientEnv :<|> checkHealth clientEnv) :<|> liftedAuthServer githubEndpoints authConfig

app :: Server Web -> Application
app handlers =
  cors (const $ Just policy) $ serve (Proxy @Web) handlers
  where
    policy =
      simpleCorsResourcePolicy

data AppConfig = AppConfig { authConfig :: Auth.Config, clientEnv :: ClientEnv }

initializeServerContext :: MonadIO m => Second -> Maybe FilePath -> m AppConfig
initializeServerContext maxInterpretationTime secrets = liftIO $ do
  putStrLn "Initializing Context"
  authConfig <- mkAuthConfig secrets
  mWebghcURL <- lookupEnv "WEBGHC_URL"
  webghcURL <- case mWebghcURL of
    Just url -> parseBaseUrl url
    Nothing -> do
      let localhost = "http://localhost:8888"
      putStrLn $ "WEBGHC_URL not set, using " <> localhost
      parseBaseUrl localhost
  manager <- newManager $ defaultManagerSettings
    { managerResponseTimeout = maybe
      (managerResponseTimeout defaultManagerSettings)
      responseTimeoutMicro . toIntegralSized
      $ toMicroseconds maxInterpretationTime
    }
  let clientEnv = mkClientEnv manager webghcURL
  pure $ AppConfig authConfig clientEnv

mkAuthConfig :: MonadIO m => Maybe FilePath -> m Auth.Config
mkAuthConfig (Just path) = do
  mConfig <- liftIO $ decodeFileStrict path
  case mConfig of
    Just config -> pure config
    Nothing -> do
      liftIO $ putStrLn $ "failed to decode " <> path
      mkAuthConfig Nothing
mkAuthConfig Nothing = liftIO $ do
  putStrLn "Initializing Context"
  githubClientId <- getEnvOrEmpty "GITHUB_CLIENT_ID"
  githubClientSecret <- getEnvOrEmpty "GITHUB_CLIENT_SECRET"
  jwtSignature <- getEnvOrEmpty "JWT_SIGNATURE"
  frontendURL <- getEnvOrEmpty "FRONTEND_URL"
  cbPath <- getEnvOrEmpty "GITHUB_CALLBACK_PATH"
  pure Auth.Config
          { _configJWTSignature = JWT.hmacSecret jwtSignature,
            _configFrontendUrl = frontendURL,
            _configGithubCbPath = cbPath,
            _configGithubClientId = OAuthClientId githubClientId,
            _configGithubClientSecret = OAuthClientSecret githubClientSecret
          }

getEnvOrEmpty :: String -> IO Text
getEnvOrEmpty name = do
  mEnv <- lookupEnv name
  case mEnv of
    Just env -> pure $ Text.pack env
    Nothing -> do
      putStrLn $ "Warning: " <> name <> " not set"
      pure mempty

initializeApplication :: AppConfig -> IO Application
initializeApplication config = do
  handlers <- mkHandlers config
  pure $ app handlers
