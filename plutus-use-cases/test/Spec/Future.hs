{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Future(tests, theFuture, increaseMarginTrace, settleEarlyTrace, payOutTrace) where

import           Control.Monad           (void)
import           Data.Default            (Default (def))
import           Test.Tasty
import qualified Test.Tasty.HUnit        as HUnit

import           Spec.TokenAccount       (assertAccountBalance)

import qualified Ledger.Ada              as Ada
import           Ledger.Crypto           (PrivateKey, PubKey (..))
import           Ledger.Oracle           (Observation (..), SignedMessage)
import qualified Ledger.Oracle           as Oracle
import           Ledger.Time             (POSIXTime)
import qualified Ledger.TimeSlot         as TimeSlot
import           Ledger.Value            (Value, scale)

import           Plutus.Contract.Test
import           Plutus.Contracts.Future (Future (..), FutureAccounts (..), FutureError, FutureSchema, FutureSetup (..),
                                          Role (..))
import qualified Plutus.Contracts.Future as F
import           Plutus.Trace.Emulator   (ContractHandle, EmulatorTrace)
import qualified Plutus.Trace.Emulator   as Trace
import qualified PlutusTx

tests :: TestTree
tests =
    testGroup "futures"
    [ checkPredicate "setup tokens"
        (assertDone (F.setupTokens @() @FutureSchema @FutureError) (Trace.walletInstanceTag w1) (const True) "setupTokens")
        $ void F.setupTokensTrace

    , checkPredicate "can initialise and obtain tokens"
        (walletFundsChange w1 (scale (-1) (F.initialMargin (theFuture startTime)) <> F.tokenFor Short F.testAccounts)
        .&&. walletFundsChange w2 (scale (-1) (F.initialMargin (theFuture startTime)) <> F.tokenFor Long F.testAccounts))
        (void (initContract startTime >> joinFuture startTime))

    , checkPredicate "can increase margin"
        (assertAccountBalance (ftoShort F.testAccounts) (== Ada.lovelaceValueOf 1936)
        .&&. assertAccountBalance (ftoLong F.testAccounts) (== Ada.lovelaceValueOf 2410))
        (increaseMarginTrace startTime)

    , checkPredicate "can settle early"
        (assertAccountBalance (ftoShort F.testAccounts) (== Ada.lovelaceValueOf 0)
        .&&. assertAccountBalance (ftoLong F.testAccounts) (== Ada.lovelaceValueOf 4246))
        (settleEarlyTrace startTime)

     , checkPredicate "can pay out"
        (assertAccountBalance (ftoShort F.testAccounts) (== Ada.lovelaceValueOf 1936)
        .&&. assertAccountBalance (ftoLong F.testAccounts) (== Ada.lovelaceValueOf 2310))
        (payOutTrace startTime)

    , goldenPir "test/Spec/future.pir" $$(PlutusTx.compile [|| F.futureStateMachine ||])

    , HUnit.testCaseSteps "script size is reasonable" $ \step ->
        reasonable' step (F.validator (theFuture startTime) F.testAccounts) 63000
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

setup :: POSIXTime -> FutureSetup
setup startTime =
    FutureSetup
        { shortPK = walletPubKey w1
        , longPK = walletPubKey w2
        , contractStart = startTime + 15000
        }

w1 :: Wallet
w1 = Wallet 1

w2 :: Wallet
w2 = Wallet 2

w10 :: Wallet
w10 = Wallet 10

-- | A futures contract over 187 units with a forward price of 1233 Lovelace,
--   due at slot #100.
theFuture :: POSIXTime -> Future
theFuture startTime = Future {
    ftDeliveryDate  = startTime + 100000,
    ftUnits         = units,
    ftUnitPrice     = forwardPrice,
    ftInitialMargin = Ada.lovelaceValueOf 800,
    ftPriceOracle   = snd oracleKeys,
    ftMarginPenalty = penalty
    }

increaseMarginTrace :: POSIXTime -> EmulatorTrace ()
increaseMarginTrace startTime = do
    _ <- initContract startTime
    hdl2 <- joinFuture startTime
    _ <- Trace.waitNSlots 20
    increaseMargin hdl2
    _ <- Trace.waitUntilSlot 100
    payOut startTime hdl2

settleEarlyTrace :: POSIXTime -> EmulatorTrace ()
settleEarlyTrace startTime = do
    _ <- initContract startTime
    hdl2 <- joinFuture startTime
    _ <- Trace.waitNSlots 20
    settleEarly startTime hdl2

payOutTrace :: POSIXTime -> EmulatorTrace ()
payOutTrace startTime = do
    _ <- initContract startTime
    hdl2 <- joinFuture startTime
    _ <- Trace.waitUntilSlot 100
    payOut startTime hdl2

-- | After this trace, the initial margin of wallet 1, and the two tokens,
--   are locked by the contract.
initContract :: POSIXTime -> EmulatorTrace (ContractHandle () FutureSchema FutureError)
initContract startTime = do
    hdl1 <- Trace.activateContractWallet w1 (F.futureContract (theFuture startTime))
    Trace.callEndpoint @"initialise-future" hdl1 (setup startTime, Short)
    _ <- Trace.waitNSlots 3
    pure hdl1

-- | Calls the "join-future" endpoint for wallet 2 and processes
--   all resulting transactions.
joinFuture :: POSIXTime -> EmulatorTrace (ContractHandle () FutureSchema FutureError)
joinFuture startTime = do
    hdl2 <- Trace.activateContractWallet w2 (F.futureContract (theFuture startTime))
    Trace.callEndpoint @"join-future" hdl2 (F.testAccounts, setup startTime)
    _ <- Trace.waitNSlots 2
    pure hdl2

-- | Calls the "settle-future" endpoint for wallet 2 and processes
--   all resulting transactions.
payOut :: POSIXTime -> ContractHandle () FutureSchema FutureError -> EmulatorTrace ()
payOut startTime hdl = do
    let
        spotPrice = Ada.lovelaceValueOf 1124
        ov = mkSignedMessage (ftDeliveryDate (theFuture startTime)) spotPrice
    Trace.callEndpoint @"settle-future" hdl ov
    void $ Trace.waitNSlots 2

-- | Margin penalty
penalty :: Value
penalty = Ada.lovelaceValueOf 1000

-- | The forward price agreed at the beginning of the contract.
forwardPrice :: Value
forwardPrice = Ada.lovelaceValueOf 1123

-- | How many units of the underlying asset are covered by the contract.
units :: Integer
units = 187

oracleKeys :: (PrivateKey, PubKey)
oracleKeys = (walletPrivKey w10, walletPubKey w10)

-- | Increase the margin of the 'Long' role by 100 lovelace
increaseMargin :: ContractHandle () FutureSchema FutureError -> EmulatorTrace ()
increaseMargin hdl = do
    Trace.callEndpoint @"increase-margin" hdl (Ada.lovelaceValueOf 100, Long)
    void $ Trace.waitNSlots 2

-- | Call 'settleEarly' with a high spot price (11240 lovelace)
settleEarly :: POSIXTime -> ContractHandle () FutureSchema FutureError -> EmulatorTrace ()
settleEarly startTime hdl = do
    let
        spotPrice = Ada.lovelaceValueOf 11240
        ov = mkSignedMessage (startTime + 25000) spotPrice
    Trace.callEndpoint @"settle-early" hdl ov
    void $ Trace.waitNSlots 1

mkSignedMessage :: POSIXTime -> Value -> SignedMessage (Observation Value)
mkSignedMessage time vl = Oracle.signObservation time vl (fst oracleKeys)
