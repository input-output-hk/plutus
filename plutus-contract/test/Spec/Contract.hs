{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Spec.Contract(tests, loopCheckpointContract, initial, upd) where

import           Control.Lens
import           Control.Monad                        (forM_, forever, void)
import           Control.Monad.Error.Lens
import           Control.Monad.Except                 (catchError, throwError)
import           Control.Monad.Freer                  (Eff)
import           Control.Monad.Freer.Extras.Log       (LogLevel (..))
import qualified Control.Monad.Freer.Extras.Log       as Log
import           Test.Tasty

import           Ledger                               (Address, PubKey, Slot)
import qualified Ledger
import qualified Ledger.Ada                           as Ada
import qualified Ledger.Constraints                   as Constraints
import qualified Ledger.Crypto                        as Crypto
import           Plutus.Contract                      as Con
import qualified Plutus.Contract.State                as State
import           Plutus.Contract.Test
import           Plutus.Contract.Types                (ResumableResult (..), responses)
import           Plutus.Contract.Util                 (loopM)
import qualified Plutus.Trace                         as Trace
import           Plutus.Trace.Emulator                (ContractInstanceTag, Emulator, EmulatorTrace, activateContract,
                                                       activeEndpoints, callEndpoint)
import           Plutus.Trace.Emulator.Types          (ContractInstanceLog (..), ContractInstanceMsg (..),
                                                       ContractInstanceState (..), UserThreadMsg (..))
import qualified PlutusTx
import           PlutusTx.Lattice
import           Prelude                              hiding (not)
import qualified Prelude                              as P
import qualified Wallet.Emulator                      as EM

import           Plutus.Contract.Effects              (ActiveEndpoint (..))
import qualified Plutus.Contract.Request              as Endpoint
import           Plutus.Contract.Resumable            (IterationID, Response (..))
import           Plutus.Contract.Trace.RequestHandler (maybeToHandler)

tests :: TestTree
tests =
    let run :: Slot -> String -> TracePredicate -> EmulatorTrace () -> _
        run sl = checkPredicateOptions (defaultCheckOptions & maxSlot .~ sl & minLogLevel .~ Debug)

        check :: Slot -> String -> Contract () Schema ContractError () -> _ -> _
        check sl nm contract pred = run sl nm (pred contract) (void $ activateContract w1 contract tag)

        tag :: ContractInstanceTag
        tag = "instance 1"

    in
    testGroup "contracts"
        [ check 1 "awaitSlot" (void $ awaitSlot 10) $ \con ->
            waitingForSlot con tag 10

        , check 1 "selectEither" (void $ selectEither (awaitSlot 10) (awaitSlot 5)) $ \con ->
            waitingForSlot con tag 5

        , check 1 "both" (void $ Con.both (awaitSlot 10) (awaitSlot 20)) $ \con ->
            waitingForSlot con tag 10

        , check 1 "both (2)" (void $ Con.both (awaitSlot 10) (awaitSlot 20)) $ \con ->
            waitingForSlot con tag 20

        , check 1 "watchAddressUntilSlot" (void $ watchAddressUntilSlot someAddress 5) $ \con ->
            waitingForSlot con tag 5

        , check 1 "endpoint" (void $ endpoint @"ep" pure) $ \con ->
            endpointAvailable @"ep" con tag

        , check 1 "forever" (forever $ endpoint @"ep" pure) $ \con ->
            endpointAvailable @"ep" con tag

        , let
            oneTwo :: Contract () Schema ContractError (Waited Int) = endpoint @"1" pure >> endpoint @"2" pure >> endpoint @"4" pure
            oneThree :: Contract () Schema ContractError (Waited Int) = endpoint @"1" pure >> endpoint @"3" pure >> endpoint @"4" pure
            con = void (oneTwo `select` oneThree)
          in
            run 1 "alternative"
                (endpointAvailable @"3" con tag
                    .&&. not (endpointAvailable @"2" con tag))
                $ do
                    hdl <- activateContract w1 con tag
                    callEndpoint @"1" hdl 1

        , let theContract :: Contract () Schema ContractError () = void $ endpoint @"1" @Int pure >> endpoint @"2" @Int pure
          in run 1 "call endpoint (1)"
                (endpointAvailable @"1" theContract tag)
                (void $ activateContract w1 theContract tag)

        , let theContract :: Contract () Schema ContractError () = void $ endpoint @"1" @Int pure >> endpoint @"2" @Int pure
          in run 1 "call endpoint (2)"
                (endpointAvailable @"2" theContract tag
                    .&&. not (endpointAvailable @"1" theContract tag))
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 1)

        , let theContract :: Contract () Schema ContractError () = void $ endpoint @"1" @Int pure >> endpoint @"2" @Int pure
          in run 1 "call endpoint (3)"
                (not (endpointAvailable @"2" theContract tag)
                    .&&. not (endpointAvailable @"1" theContract tag))
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 2)

        , let theContract :: Contract () Schema ContractError [ActiveEndpoint] = getWaited <$> endpoint @"5" @[ActiveEndpoint] pure
              expected = ActiveEndpoint{ aeDescription = EndpointDescription "5", aeMetadata = Nothing}
          in run 5 "active endpoints"
                (assertDone theContract tag ((==) [expected]) "should be done")
                $ do
                    hdl <- activateContract w1 theContract tag
                    _ <- Trace.waitNSlots 1
                    eps <- activeEndpoints hdl
                    void $ callEndpoint @"5" hdl eps

        , let theContract :: Contract () Schema ContractError () = void $ submitTx mempty >> watchAddressUntilSlot someAddress 20
          in run 1 "submit tx"
                (waitingForSlot theContract tag 20)
                (void $ activateContract w1 theContract tag)

        , let smallTx = Constraints.mustPayToPubKey (Crypto.pubKeyHash $ walletPubKey (Wallet 2)) (Ada.lovelaceValueOf 10)
              theContract :: Contract () Schema ContractError (Waited ()) = submitTx smallTx >>= awaitTxConfirmed . Ledger.txId >> submitTx smallTx >>= awaitTxConfirmed . Ledger.txId
          in run 3 "handle several blockchain events"
                (walletFundsChange w1 (Ada.lovelaceValueOf (-20))
                    .&&. assertNoFailedTransactions
                    .&&. assertDone theContract tag (const True) "all blockchain events should be processed")
                (void $ activateContract w1 theContract tag >> Trace.waitUntilSlot 3)

        , let l = endpoint @"1" pure >> endpoint @"2" pure
              r = endpoint @"3" pure >> endpoint @"4" pure
              theContract :: Contract () Schema ContractError () = void $ selectEither l r
          in run 1 "select either"
                (assertDone theContract tag (const True) "left branch should finish")
                (activateContract w1 theContract tag >>= (\hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 2))

        , let theContract :: Contract () Schema ContractError () = void $ loopM (\_ -> Left . getWaited <$> endpoint @"1" @Int pure) 0
          in run 1 "loopM"
                (endpointAvailable @"1" theContract tag)
                (void $ activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 1)

        , let theContract :: Contract () Schema ContractError () = void $ throwing Con._ContractError $ OtherError "error"
          in run 1 "throw an error"
                (assertContractError theContract tag (\case { OtherError "error" -> True; _ -> False}) "failed to throw error")
                (void $ activateContract w1 theContract tag)

        , run 2 "pay to wallet"
            (walletFundsChange w1 (Ada.lovelaceValueOf (-200))
                .&&. walletFundsChange w2 (Ada.lovelaceValueOf 200)
                .&&. assertNoFailedTransactions)
            (void $ Trace.payToWallet w1 w2 (Ada.lovelaceValueOf 200))

        , let theContract :: Contract () Schema ContractError PubKey = ownPubKey
          in run 1 "own public key"
                (assertDone theContract tag (== walletPubKey w2) "should return the wallet's public key")
                (void $ activateContract w2 (void theContract) tag)

        , let payment = Constraints.mustPayToPubKey (Crypto.pubKeyHash $ walletPubKey w2) (Ada.lovelaceValueOf 10)
              theContract :: Contract () Schema ContractError (Waited ()) = submitTx payment >>= awaitTxConfirmed . Ledger.txId
          in run 2 "await tx confirmed"
            (assertDone theContract tag (const True) "should be done")
            (activateContract w1 theContract tag >> void (Trace.waitNSlots 1))

        , run 1 "checkpoints"
            (not (endpointAvailable @"2" checkpointContract tag) .&&. endpointAvailable @"1" checkpointContract tag)
            (void $ activateContract w1 checkpointContract tag >>= \hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 1)

        , run 1 "error handling & checkpoints"
            (assertDone errorContract tag (\i -> getWaited i == 11) "should finish")
            (void $ activateContract w1 (void errorContract) tag >>= \hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 10 >> callEndpoint @"3" hdl 11)

        , run 1 "loop checkpoint"
            (assertDone loopCheckpointContract tag (\i -> i == 4) "should finish"
            .&&. assertResumableResult loopCheckpointContract tag DoShrink (null . view responses) "should collect garbage"
            .&&. assertResumableResult loopCheckpointContract tag DontShrink ((==) 4 . length . view responses) "should keep everything"
            )
            $ do
                hdl <- activateContract w1 loopCheckpointContract tag
                forM_ [1..4] (\_ -> callEndpoint @"1" hdl 1)

        , let theContract :: Contract () Schema ContractError (Waited ()) = logInfo @String "waiting for endpoint 1" >> endpoint @"1" (logInfo . (<>) "Received value: " . show)
              matchLogs :: [EM.EmulatorTimeEvent ContractInstanceLog] -> Bool
              matchLogs lgs =
                  case _cilMessage . EM._eteEvent <$> lgs of
                            [ Started, ContractLog "waiting for endpoint 1", CurrentRequests [_], ReceiveEndpointCall{}, ContractLog "Received value: 27", HandledRequest _, CurrentRequests [], StoppedNoError] -> True
                            _ -> False

          in run 1 "contract logs"
                (assertInstanceLog tag matchLogs)
                (void $ activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 27)

        , let theContract :: Contract () Schema ContractError (Waited ()) = logInfo @String "waiting for endpoint 1" >> endpoint @"1" (logInfo . (<>) "Received value: " . show)
              matchLogs :: [EM.EmulatorTimeEvent UserThreadMsg] -> Bool
              matchLogs lgs =
                  case EM._eteEvent <$> lgs of
                            [ UserLog "Received contract state", UserLog "Final state: Right Nothing"] -> True
                            _                                                                          -> False

          in run 4 "contract state"
                (assertUserLog matchLogs)
                $ do
                    hdl <- Trace.activateContractWallet w1 theContract
                    Trace.waitNSlots 1
                    ContractInstanceState{instContractState=ResumableResult{_finalState}} <- Trace.getContractState hdl
                    Log.logInfo @String "Received contract state"
                    Log.logInfo @String $ "Final state: " <> show _finalState

        ]

w1 :: EM.Wallet
w1 = EM.Wallet 1

w2 :: EM.Wallet
w2 = EM.Wallet 2

checkpointContract :: Contract () Schema ContractError ()
checkpointContract = void $ do
    checkpoint $ do
        endpoint @"1" @Int pure
        endpoint @"2" @Int pure
    checkpoint $ do
        endpoint @"1" @Int pure
        endpoint @"3" @Int pure

loopCheckpointContract :: Contract () Schema ContractError Int
loopCheckpointContract = do
    -- repeatedly expose the "1" endpoint until we get a total
    -- value greater than 3.
    -- We can call "1" with different values to control whether
    -- the left or right branch is chosen.
    flip checkpointLoop (0 :: Int) $ \counter -> fmap getWaited $ endpoint @"1" @Int $ \vl -> do
        let newVal = counter + vl
        if newVal > 3
            then pure (Left newVal)
            else pure (Right newVal)

errorContract :: Contract () Schema ContractError (Waited Int)
errorContract = do
    catchError
        (endpoint @"1" @Int $ \_ -> throwError (OtherError "something went wrong"))
        (\_ -> checkpoint $ endpoint @"2" @Int pure >> endpoint @"3" @Int pure)

someAddress :: Address
someAddress = Ledger.scriptAddress $
    Ledger.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

type Schema =
    Endpoint "1" Int
        .\/ Endpoint "2" Int
        .\/ Endpoint "3" Int
        .\/ Endpoint "4" Int
        .\/ Endpoint "ep" ()
        .\/ Endpoint "5" [ActiveEndpoint]

initial :: _
initial = State.initialiseContract loopCheckpointContract

upd :: _
upd = State.insertAndUpdateContract loopCheckpointContract
