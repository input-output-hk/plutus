{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Plutus.PAB.CoreSpec
    ( tests
    , stopContractInstanceTest
    , walletFundsChangeTest
    , observableStateChangeTest
    ) where

import           Control.Lens                             ((&), (+~), (^.))
import           Control.Monad                            (unless, void)
import           Control.Monad.Freer                      (Eff, Member, Members)
import           Control.Monad.Freer.Error                (Error, throwError)
import           Control.Monad.Freer.Extras.Log           (LogMsg)
import qualified Control.Monad.Freer.Extras.Log           as EmulatorLog
import           Control.Monad.Freer.Extras.State         (use)
import           Control.Monad.Freer.State                (State)
import           Control.Monad.IO.Class                   (MonadIO (..))
import qualified Data.Aeson                               as JSON
import           Data.Foldable                            (fold, traverse_)

import qualified Data.Aeson.Types                         as JSON
import           Data.Either                              (isRight)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (isJust)
import qualified Data.Monoid                              as M
import           Data.Proxy                               (Proxy (..))
import           Data.Semigroup                           (Last (..))
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Data.Text.Extras                         (tshow)
import           Ledger                                   (pubKeyAddress)
import           Ledger.Ada                               (adaSymbol, adaToken, lovelaceValueOf)
import qualified Ledger.Ada                               as Ada
import qualified Ledger.AddressMap                        as AM
import           Ledger.Value                             (valueOf)
import           Plutus.Contract.State                    (ContractResponse (..))
import           Plutus.Contracts.Currency                (OneShotCurrency, SimpleMPS (..))
import qualified Plutus.Contracts.GameStateMachine        as Contracts.GameStateMachine
import           Plutus.Contracts.PingPong                (PingPongState (..))
import           Plutus.PAB.Core                          as Core
import           Plutus.PAB.Core.ContractInstance         (ContractInstanceMsg)
import           Plutus.PAB.Core.ContractInstance.STM     (BlockchainEnv (..))
import           Plutus.PAB.Effects.Contract              (ContractEffect, serialisableState)
import           Plutus.PAB.Effects.Contract.Builtin      (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin      as Builtin
import           Plutus.PAB.Effects.Contract.ContractTest (TestContracts (..))
import           Plutus.PAB.Events.ContractInstanceState  (PartiallyDecodedResponse)
import           Plutus.PAB.Simulator                     (Simulation, TxCounts (..))
import qualified Plutus.PAB.Simulator                     as Simulator
import qualified Plutus.PAB.Simulator.Test                as Simulator
import           Plutus.PAB.Types                         (PABError (..), chainOverviewBlockchain, mkChainOverview)
import qualified Plutus.PAB.Webserver.WebSocket           as WS
import           PlutusTx.Monoid                          (Group (inv))
import           Test.QuickCheck.Instances.UUID           ()
import           Test.Tasty                               (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit                         (testCase)
import           Wallet.API                               (WalletAPIError, ownPubKey)
import qualified Wallet.API                               as WAPI
import qualified Wallet.Emulator.Chain                    as Chain
import           Wallet.Emulator.Wallet                   (Wallet (..))
import           Wallet.Rollup                            (doAnnotateBlockchain)
import           Wallet.Rollup.Types                      (DereferencedInput, dereferencedInputs, isFound)
import           Wallet.Types                             (ContractInstanceId)

tests :: TestTree
tests = testGroup "Plutus.PAB.Core" [activateContractTests, executionTests]

runScenario :: Simulation (Builtin TestContracts) a -> IO ()
runScenario sim = do
    result <- Simulator.runSimulation sim
    case result of
        Left err -> error (show err)
        Right _  -> pure ()

defaultWallet :: Wallet
defaultWallet = Wallet 1

activateContractTests :: TestTree
activateContractTests =
    testGroup
        "activateContract scenario"
        [ testCase "Initially there are no contracts active" $
            runScenario $ do
                active <- Simulator.activeContracts
                assertEqual "" 0 $ Set.size active
        , testCase "We can activate a contract" $
          runScenario $ do
              void $ Simulator.activateContract defaultWallet GameStateMachine
              --
              active <- Simulator.activeContracts
              assertEqual "" 1 $ Set.size active
        ]

executionTests :: TestTree
executionTests =
    testGroup
        "Executing contracts."
        [ guessingGameTest
        , currencyTest
        , testCase "wait for update" waitForUpdateTest
        , testCase "stop contract instance" stopContractInstanceTest
        , testCase "can subscribe to slot updates" slotChangeTest
        , testCase "can subscribe to wallet funds changes" walletFundsChangeTest
        , testCase "can subscribe to observable state changes" observableStateChangeTest
        ]

waitForUpdateTest :: IO ()
waitForUpdateTest =
    runScenario $ do
        let is :: PingPongState -> JSON.Value -> Maybe PingPongState
            is st vl = do
                case JSON.parseEither JSON.parseJSON vl of
                    Right (M.Last (Just st')) | st == st' -> Just st'
                    _                                     -> Nothing
        p1 <- Simulator.activateContract defaultWallet PingPong
        p2 <- Simulator.activateContract defaultWallet PingPong
        void $ Simulator.callEndpointOnInstance p1 "initialise" ()
        Simulator.waitNSlots 5
        _ <- Simulator.waitForEndpoint p1 "wait"
        void $ Simulator.callEndpointOnInstance p1 "wait" ()
        void $ Simulator.callEndpointOnInstance p2 "pong" ()
        _ <- Simulator.waitForState (is Ponged) p1
        void $ Simulator.callEndpointOnInstance p1 "wait" ()
        _ <- Simulator.waitForEndpoint p2 "ping"
        void $ Simulator.callEndpointOnInstance p2 "ping" ()
        _ <- Simulator.waitForState (is Pinged) p1
        pure ()

stopContractInstanceTest :: IO ()
stopContractInstanceTest = do
    runScenario $ do
        p1 <- Simulator.activateContract defaultWallet PingPong
        _ <- Simulator.stopInstance p1
        _ <- Simulator.waitUntilFinished p1
        st <- Simulator.instanceActivity p1
        assertEqual "Instance should be 'Stopped'" st Simulator.Stopped

slotChangeTest :: IO ()
slotChangeTest = runScenario $ do
    env <- Core.askBlockchainEnv @(Builtin TestContracts) @(Simulator.SimulatorState (Builtin TestContracts))
    let stream = WS.slotChange env
    ns <- liftIO (WS.readN 5 stream)
    assertEqual "Should wait for five slots" 5 (length ns)

walletFundsChangeTest :: IO ()
walletFundsChangeTest = runScenario $ do
    let initialBalance = lovelaceValueOf 10_000_000_000
        payment = lovelaceValueOf 50
        fee     = lovelaceValueOf 10 -- TODO: Calculate the fee from the tx

    env <- Core.askBlockchainEnv @(Builtin TestContracts) @(Simulator.SimulatorState (Builtin TestContracts))
    let stream = WS.walletFundsChange defaultWallet env
    (initialValue, next) <- liftIO (WS.readOne stream)
    (wllt, pk) <- Simulator.addWallet
    _ <- Simulator.payToPublicKey defaultWallet pk payment
    nextStream <- case next of { Nothing -> throwError (OtherError "no next value"); Just a -> pure a; }
    (finalValue, _) <- liftIO (WS.readOne nextStream)
    let difference = initialValue <> inv finalValue
    assertEqual "defaultWallet should make a payment" difference (payment <> fee)

    -- Check that the funds are correctly registered in the newly created wallet
    let stream2 = WS.walletFundsChange wllt env
    vl2 <- liftIO (WS.readN 1 stream2) >>= \case { [newVal] -> pure newVal; _ -> throwError (OtherError "newVal not found")}
    assertEqual "generated wallet should receive a payment" (initialBalance <> payment) vl2

observableStateChangeTest :: IO ()
observableStateChangeTest = runScenario $ do
    let getCurrency :: JSON.Value -> Maybe OneShotCurrency
        getCurrency vl = do
            case JSON.parseEither JSON.parseJSON vl of
                Right (Just (Last cur)) -> Just cur
                _                       -> Nothing
    env <- Core.askInstancesState @(Builtin TestContracts) @(Simulator.SimulatorState (Builtin TestContracts))
    instanceId <- Simulator.activateContract defaultWallet Currency
    createCurrency instanceId SimpleMPS{tokenName="my token", amount = 10000}
    let stream = WS.observableStateChange instanceId env
    vl2 <- liftIO (WS.readN 2 stream) >>= \case { [_, newVal] -> pure newVal; _ -> throwError (OtherError "newVal not found")}
    assertBool "observable state should change" (isJust $ getCurrency vl2)

currencyTest :: TestTree
currencyTest =
    let mps = SimpleMPS{tokenName="my token", amount = 10000}
        getCurrency :: JSON.Value -> Maybe OneShotCurrency
        getCurrency vl = do
            case JSON.parseEither JSON.parseJSON vl of
                Right (Just (Last cur)) -> Just cur
                _                       -> Nothing
    in
    testCase "Currency" $
        runScenario $ do
              initialTxCounts <- Simulator.txCounts
              instanceId <- Simulator.activateContract defaultWallet Currency
              assertTxCounts
                  "Activating the currency contract does not generate transactions."
                  initialTxCounts
              createCurrency instanceId mps
              result <- Simulator.waitForState getCurrency instanceId
              assertTxCounts
                "Minting the currency should produce one valid transaction."
                (initialTxCounts & Simulator.txValidated +~ 1)

guessingGameTest :: TestTree
guessingGameTest =
    testCase "Guessing Game" $
          runScenario $ do
              let openingBalance = 100_000_000_000
                  lockAmount = 15
                  walletFundsChange msg delta = do
                        address <- pubKeyAddress <$> Simulator.handleAgentThread defaultWallet ownPubKey
                        balance <- Simulator.valueAt address
                        fees <- Simulator.walletFees defaultWallet
                        assertEqual msg
                            (openingBalance + delta)
                            (valueOf (balance <> fees) adaSymbol adaToken)
              initialTxCounts <- Simulator.txCounts
              walletFundsChange "Check our opening balance." 0
              -- need to add contract address to wallet's watched addresses
              instanceId <- Simulator.activateContract defaultWallet GameStateMachine

              assertTxCounts
                  "Activating the game does not generate transactions."
                  initialTxCounts
              lock
                  instanceId
                  Contracts.GameStateMachine.LockArgs
                      { Contracts.GameStateMachine.lockArgsValue = lovelaceValueOf lockAmount
                      , Contracts.GameStateMachine.lockArgsSecret = "password"
                      }

              assertTxCounts
                  "Locking the game state machine should produce two transactions"
                  (initialTxCounts & Simulator.txValidated +~ 2)
              walletFundsChange "Locking the game should reduce our balance." (negate lockAmount)
              game1Id <- Simulator.activateContract defaultWallet GameStateMachine

              guess
                  game1Id
                  Contracts.GameStateMachine.GuessArgs
                      { Contracts.GameStateMachine.guessArgsNewSecret = "wrong"
                      , Contracts.GameStateMachine.guessArgsOldSecret = "wrong"
                      , Contracts.GameStateMachine.guessArgsValueTakenOut = lovelaceValueOf lockAmount
                      }

              _ <- Simulator.waitNSlots 5
              assertTxCounts
                "A wrong guess does not produce a valid transaction on the chain."
                (initialTxCounts & Simulator.txValidated +~ 2)
              game2Id <- Simulator.activateContract defaultWallet GameStateMachine

              guess
                  game2Id
                  Contracts.GameStateMachine.GuessArgs
                      { Contracts.GameStateMachine.guessArgsNewSecret = "password"
                      , Contracts.GameStateMachine.guessArgsOldSecret = "password"
                      , Contracts.GameStateMachine.guessArgsValueTakenOut = lovelaceValueOf lockAmount
                      }

              assertTxCounts
                "A correct guess creates a third transaction."
                (initialTxCounts & Simulator.txValidated +~ 3)
              walletFundsChange "The wallet should now have its money back." 0
              blocks <- Simulator.blockchain
              assertBool
                  "We have some confirmed blocks in this test."
                  (not (null (mconcat blocks)))
              let chainOverview = mkChainOverview (reverse blocks)
              annotatedBlockchain <-
                      doAnnotateBlockchain
                          (chainOverviewBlockchain chainOverview)
              let allDereferencedInputs :: [DereferencedInput]
                  allDereferencedInputs =
                      mconcat $
                      dereferencedInputs <$> mconcat annotatedBlockchain
              assertBool
                  "Full TX history can be annotated."
                  (all isFound allDereferencedInputs)

assertTxCounts ::
    Text
    -> TxCounts
    -> Simulation (Builtin TestContracts) ()
assertTxCounts msg expected = do
    Simulator.waitForValidatedTxCount (expected ^. Simulator.txValidated)
    Simulator.txCounts >>= assertEqual msg expected

assertDone ::
    Wallet
    -> ContractInstanceId
    -> Simulation (Builtin TestContracts) ()
assertDone wallet i = do
    ContractResponse{hooks} <- serialisableState (Proxy @(Builtin TestContracts)) <$> Simulator.instanceState wallet i
    case hooks of
        [] -> pure ()
        xs ->
            throwError
                $ OtherError
                $ Text.unwords
                    [ "Contract"
                    , tshow i
                    , "not done. Open requests:"
                    , tshow xs
                    ]

lock ::
    ContractInstanceId
    -> Contracts.GameStateMachine.LockArgs
    -> Simulation (Builtin TestContracts) ()
lock uuid params = do
    let ep = "lock"
    _ <- Simulator.waitForEndpoint uuid ep
    void $ Simulator.callEndpointOnInstance uuid ep params

guess ::
    ContractInstanceId
    -> Contracts.GameStateMachine.GuessArgs
    -> Simulation (Builtin TestContracts) ()
guess uuid params = do
    let ep = "guess"
    _ <- Simulator.waitForEndpoint uuid ep
    void $ Simulator.callEndpointOnInstance uuid ep params

callAdder ::
    ContractInstanceId
    -> ContractInstanceId
    -> Simulation (Builtin TestContracts) ()
callAdder source target = do
    let ep = "target instance"
    _ <- Simulator.waitForEndpoint source ep
    void $ Simulator.callEndpointOnInstance source ep target

-- | Call the @"Create native token"@ endpoint on the currency contract.
createCurrency ::
    ContractInstanceId
    -> SimpleMPS
    -> Simulation (Builtin TestContracts) ()
createCurrency uuid value = do
    let ep = "Create native token"
    _ <- Simulator.waitForEndpoint uuid ep
    void $ Simulator.callEndpointOnInstance uuid ep value

assertEqual ::
    forall a effs.
    ( Eq a
    , Show a
    , Member (Error PABError) effs
    )
    => Text
    -> a
    -> a
    -> Eff effs ()
assertEqual msg expected actual =
    unless (expected == actual)
        $ throwError
        $ OtherError
        $ Text.unwords
            [ msg
            , "Expected: " <> tshow expected
            , "Actual: " <> tshow actual
            ]

assertBool ::
    forall effs.
    ( Member (Error PABError) effs
    )
    => Text
    -> Bool
    -> Eff effs ()
assertBool msg b =
    unless b
        $ throwError
        $ OtherError
        $ Text.unwords
            [ msg
            , "failed"
            ]
