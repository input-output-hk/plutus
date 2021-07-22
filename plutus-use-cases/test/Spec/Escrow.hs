{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Spec.Escrow(tests, redeemTrace, redeem2Trace, refundTrace) where

import           Control.Monad           (void)
import           Data.Default            (Default (def))

import           Ledger                  (POSIXTime, pubKeyHash)
import qualified Ledger.Ada              as Ada
import qualified Ledger.TimeSlot         as TimeSlot
import qualified Ledger.Typed.Scripts    as Scripts
import           Plutus.Contract
import           Plutus.Contract.Test

import           Plutus.Contracts.Escrow
import qualified Plutus.Trace.Emulator   as Trace

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HUnit

tests :: TestTree
tests = testGroup "escrow"
    [ let con = void $ payEp @() @EscrowSchema @EscrowError (escrowParams startTime) in
      checkPredicate "can pay"
        ( assertDone con (Trace.walletInstanceTag w1) (const True) "escrow pay not done"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        )
        $ do
          hdl <- Trace.activateContractWallet w1 con
          Trace.callEndpoint @"pay-escrow" hdl (Ada.lovelaceValueOf 10)
          void $ Trace.waitNSlots 1

    , let con = void $ selectEither (payEp @() @EscrowSchema @EscrowError (escrowParams startTime)) (redeemEp (escrowParams startTime)) in
      checkPredicate "can redeem"
        ( assertDone con (Trace.walletInstanceTag w3) (const True) "escrow redeem not done"
          .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
          .&&. walletFundsChange w2 (Ada.lovelaceValueOf 10)
          .&&. walletFundsChange w3 mempty
        )
        (redeemTrace startTime)

    , checkPredicate "can redeem even if more money than required has been paid in"

          -- in this test case we pay in a total of 40 lovelace (10 more than required), for
          -- the same contract as before, requiring 10 lovelace to go to wallet 1 and 20 to
          -- wallet 2.
          --
          -- The scenario is
          -- * Wallet 1 contributes 20
          -- * Wallet 2 contributes 10
          -- * Wallet 3 contributes 10
          -- * Wallet 1 is going to redeem the payments
          --

          -- Wallet 1 pays 20 and receives 10 from the escrow contract and another 10
          -- in excess inputs
          ( walletFundsChange w1 (Ada.lovelaceValueOf 0)

          -- Wallet 2 pays 10 and receives 20, as per the contract.
            .&&. walletFundsChange w2 (Ada.lovelaceValueOf 10)

          -- Wallet 3 pays 10 and doesn't receive anything.
            .&&. walletFundsChange w3 (Ada.lovelaceValueOf (-10))
          )
          (redeem2Trace startTime)

    , let con = void $ payEp @()  @EscrowSchema @EscrowError (escrowParams startTime) >> refundEp (escrowParams startTime) in
      checkPredicate "can refund"
        ( walletFundsChange w1 mempty
          .&&. assertDone con (Trace.walletInstanceTag w1) (const True) "refund should succeed")
        (refundTrace startTime)

    , HUnit.testCaseSteps "script size is reasonable" $ \step -> reasonable' step (Scripts.validatorScript $ typedValidator (escrowParams startTime)) 32000
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 100999
    , escrowTargets  =
        [ payToPubKeyTarget (pubKeyHash $ walletPubKey w1) (Ada.lovelaceValueOf 10)
        , payToPubKeyTarget (pubKeyHash $ walletPubKey w2) (Ada.lovelaceValueOf 20)
        ]
    }

-- | Wallets 1 and 2 pay into an escrow contract, wallet 3
--   cashes out.
redeemTrace :: POSIXTime -> Trace.EmulatorTrace ()
redeemTrace startTime = do
    let con = void $ selectEither (payEp @() @EscrowSchema @EscrowError (escrowParams startTime)) (redeemEp (escrowParams startTime))
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    hdl3 <- Trace.activateContractWallet w3 con

    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.lovelaceValueOf 20)
    Trace.callEndpoint @"pay-escrow" hdl2 (Ada.lovelaceValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"redeem-escrow" hdl3 ()
    void $ Trace.waitNSlots 1

-- | Wallets 1-3 pay into an escrow contract, wallet 1 redeems.
redeem2Trace :: POSIXTime -> Trace.EmulatorTrace ()
redeem2Trace startTime = do
    let con = void $ both (payEp @() @EscrowSchema @EscrowError (escrowParams startTime)) (redeemEp (escrowParams startTime))
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    hdl3 <- Trace.activateContractWallet w3 con
    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.lovelaceValueOf 20)
    Trace.callEndpoint @"pay-escrow" hdl2 (Ada.lovelaceValueOf 10)
    Trace.callEndpoint @"pay-escrow" hdl3 (Ada.lovelaceValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"redeem-escrow" hdl1 ()
    void $ Trace.waitNSlots 1

-- | Wallet 1 pays into an escrow contract and gets a refund when the
--   amount isn't claimed.
refundTrace :: POSIXTime -> Trace.EmulatorTrace ()
refundTrace startTime = do
    let con = void $ payEp @() @EscrowSchema @EscrowError (escrowParams startTime) >> refundEp (escrowParams startTime)
    hdl1 <- Trace.activateContractWallet w1 con
    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.lovelaceValueOf 20)
    _ <- Trace.waitNSlots 100
    Trace.callEndpoint @"refund-escrow" hdl1 ()
    void $ Trace.waitNSlots 1
