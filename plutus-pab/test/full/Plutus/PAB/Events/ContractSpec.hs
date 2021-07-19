{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.PAB.Events.ContractSpec
    ( tests
    ) where

import           Control.Monad                           (void)
import           Control.Monad.Except                    (ExceptT (ExceptT), runExceptT)
import           Control.Monad.Trans.Except              (except)
import           Data.Aeson                              (Value)
import qualified Data.Aeson                              as JSON
import qualified Data.Aeson.Encode.Pretty                as JSON
import           Data.Bifunctor                          (first)
import qualified Data.ByteString.Char8                   as BS
import qualified Data.ByteString.Char8                   as BS8
import qualified Data.ByteString.Lazy                    as BSL
import qualified Data.ByteString.Lazy.Char8              as BSL8
import           Data.Proxy                              (Proxy (Proxy))
import qualified Data.Text                               as T
import           GHC.TypeLits                            (symbolVal)
import           Plutus.Contract                         (EmptySchema)
import           Plutus.Contract.Effects                 (ActiveEndpoint (..), PABReq (..), PABResp (..), Waited (..))
import           Plutus.Contract.Resumable               (Response (..))
import qualified Plutus.Contract.Schema                  as Schema
import           Plutus.Contract.State                   (ContractRequest (..))
import qualified Plutus.Contract.State                   as State
import           Plutus.Contracts.GameStateMachine       (GameStateMachineSchema, contract)
import           Plutus.PAB.Arbitrary                    ()
import           Plutus.PAB.ContractCLI                  (Command (Initialise, Update), runCliCommand, runPromptPure)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import qualified Plutus.PAB.Events.ContractInstanceState as ContractInstanceState
import           Test.Tasty                              (TestTree, testGroup)
import           Test.Tasty.HUnit                        (assertFailure, testCase)
import           Wallet.Types                            (EndpointDescription (EndpointDescription))

tests :: TestTree
tests = testGroup "Plutus.PAB.Events.Contract" [jsonTests]

jsonTests :: TestTree
jsonTests =
    testGroup
        "ToJSON/FromJSON"
        [ testCase "Decode handlers" $
          let handlers :: PABReq
              handlers =
                  ExposeEndpointReq $
                  ActiveEndpoint
                      { aeDescription = EndpointDescription (symbolVal (Proxy @"guess"))
                      , aeMetadata    = Nothing
                      }
              response :: Either String PABReq
              response = JSON.eitherDecode $ JSON.encode handlers
           in void (assertRight response)
        , testCase "Decode contract initialisation" $ do
            void (assertRight initialResponse)

        , testCase "Send a response to the contract" $ do
            oldState <- assertRight initialResponse
            let req :: ContractRequest JSON.Value JSON.Value
                req = ContractRequest{oldState = State.newState oldState, event = Response{rspRqID = 0, rspItID = 0, rspResponse = JSON.toJSON (AwaitSlotResp (Waited 1))}}
                input = BSL.toStrict (JSON.encodePretty req)
                v = first (foldMap BS8.unpack) $ runPromptPure (runCliCommand (Proxy @EmptySchema) (first (T.pack . show) contract) Update) input
                result = v >>= JSON.eitherDecode @ResponseType . BSL.fromStrict

            void (assertRight result)
        ]

assertRight :: Either String a -> IO a
assertRight = either assertFailure pure

type ResponseType = State.ContractResponse Value Value Value PABReq

initialResponse :: Either String ResponseType
initialResponse =
    let v = first (foldMap BS8.unpack) $ runPromptPure (runCliCommand (Proxy @EmptySchema) (first (T.pack . show) contract) Initialise) mempty
    in v >>= JSON.eitherDecode @(State.ContractResponse Value Value Value PABReq) . BSL.fromStrict
