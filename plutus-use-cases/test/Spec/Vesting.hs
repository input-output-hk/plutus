{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}
module Spec.Vesting(tests, retrieveFundsTrace, vesting) where

import           Control.Monad            (void)
import           Data.Default             (Default (def))
import           Test.Tasty
import qualified Test.Tasty.HUnit         as HUnit

import qualified Ledger
import qualified Ledger.Ada               as Ada
import qualified Ledger.TimeSlot          as TimeSlot
import           Plutus.Contract.Test
import           Plutus.Contracts.Vesting
import           Plutus.Trace.Emulator    (EmulatorTrace)
import qualified Plutus.Trace.Emulator    as Trace
import qualified PlutusTx
import qualified PlutusTx.Numeric         as Numeric
import           Prelude                  hiding (not)

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tests :: TestTree
tests =
    let con = vestingContract (vesting startTime) in
    testGroup "vesting"
    [ checkPredicate "secure some funds with the vesting script"
        (walletFundsChange w2 (Numeric.negate $ totalAmount $ vesting startTime))
        $ do
            hdl <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl ()
            void $ Trace.waitNSlots 1

    , checkPredicate "retrieve some funds"
        (walletFundsChange w2 (Numeric.negate $ totalAmount $ vesting startTime)
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf 10))
        (retrieveFundsTrace startTime)

    , checkPredicate "cannot retrieve more than allowed"
        (walletFundsChange w1 mempty
        .&&. assertContractError con (Trace.walletInstanceTag w1) (== expectedError) "error should match")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl2 ()
            Trace.waitNSlots 10
            Trace.callEndpoint @"retrieve funds" hdl1 (Ada.lovelaceValueOf 30)
            void $ Trace.waitNSlots 1

    , checkPredicate "can retrieve everything at the end"
        (walletFundsChange w1 (totalAmount $ vesting startTime)
        .&&. assertNoFailedTransactions
        .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "should be done")
        $ do
            hdl1 <- Trace.activateContractWallet w1 con
            hdl2 <- Trace.activateContractWallet w2 con
            Trace.callEndpoint @"vest funds" hdl2 ()
            Trace.waitNSlots 20
            Trace.callEndpoint @"retrieve funds" hdl1 (totalAmount $ vesting startTime)
            void $ Trace.waitNSlots 2

    , goldenPir "test/Spec/vesting.pir" $$(PlutusTx.compile [|| validate ||])
    , HUnit.testCaseSteps "script size is reasonable" $ \step -> reasonable' step (vestingScript $ vesting startTime) 33000
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

-- | The scenario used in the property tests. It sets up a vesting scheme for a
--   total of 60 lovelace over 20 blocks (20 lovelace can be taken out before
--   that, at 10 blocks).
vesting :: Ledger.POSIXTime -> VestingParams
vesting startTime =
    VestingParams
        { vestingTranche1 = VestingTranche (startTime + 10000) (Ada.lovelaceValueOf 20)
        , vestingTranche2 = VestingTranche (startTime + 20000) (Ada.lovelaceValueOf 40)
        , vestingOwner    = Ledger.pubKeyHash $ walletPubKey w1 }

retrieveFundsTrace :: Ledger.POSIXTime -> EmulatorTrace ()
retrieveFundsTrace startTime = do
    let con = vestingContract @VestingError (vesting startTime)
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    Trace.callEndpoint @"vest funds" hdl2 ()
    Trace.waitNSlots 10
    Trace.callEndpoint @"retrieve funds" hdl1 (Ada.lovelaceValueOf 10)
    void $ Trace.waitNSlots 2

expectedError :: VestingError
expectedError =
    let payment = Ada.lovelaceValueOf 30
        maxPayment = Ada.lovelaceValueOf 20
        mustRemainLocked = Ada.lovelaceValueOf 40
    in InsufficientFundsError payment maxPayment mustRemainLocked
