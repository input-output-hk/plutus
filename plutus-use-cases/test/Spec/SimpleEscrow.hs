{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Spec.SimpleEscrow(tests) where

import           Control.Lens
import           Control.Monad                 (void)
import           Data.Default                  (Default (def))

import           Ledger                        (POSIXTime, Value, pubKeyHash)
import qualified Ledger.Ada                    as Ada
import qualified Ledger.TimeSlot               as TimeSlot
import qualified Ledger.Value                  as Value
import           Plutus.Contract.Test
import           Plutus.Contracts.SimpleEscrow
import qualified Plutus.Trace.Emulator         as Trace

import           Test.Tasty

tests :: TestTree
tests = testGroup "simple-escrow"
    [ checkPredicate "can lock some value in the contract"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (-10))
          .&&. walletFundsChange w2 mempty
        )
        $ do
          let params = mkEscrowParams startTime (Ada.lovelaceValueOf 10) (Ada.lovelaceValueOf 1)

          hdl <- Trace.activateContractWallet w1 lockEp
          Trace.callEndpoint @"lock" hdl params
    , checkPredicateOptions options "can lock and redeem"
        ( walletFundsChange w1 (token1 (-10) <> token2 5)
          .&&. walletFundsChange w2 (token1 10 <> token2 (-5))
        )
        $ do
          let params = mkEscrowParams startTime (token1 10) (token2 5)

          hdl1 <- Trace.activateContractWallet w1 lockEp
          Trace.callEndpoint @"lock" hdl1 params
          void $ Trace.waitNSlots 1

          hdl2 <- Trace.activateContractWallet w2 (void redeemEp)
          void $ Trace.callEndpoint @"redeem" hdl2 params
    , checkPredicate "can lock and refund"
        ( walletFundsChange w1 mempty
          .&&. walletFundsChange w2 mempty
        )
        $ do
          let params = mkEscrowParams startTime (Ada.lovelaceValueOf 10) (Ada.lovelaceValueOf 1)

          hdl <- Trace.activateContractWallet w1 (lockEp >> void refundEp)
          Trace.callEndpoint @"lock" hdl params

          void $ Trace.waitNSlots 100
          void $ Trace.callEndpoint @"refund" hdl params
    , checkPredicate "only locking wallet can request refund"
        ( walletFundsChange w1 (Ada.lovelaceValueOf (-100))
          .&&. walletFundsChange w2 mempty
        )
        $ do
          let params = mkEscrowParams startTime (Ada.lovelaceValueOf 100) (Ada.lovelaceValueOf 1)

          hdl1 <- Trace.activateContractWallet w1 lockEp
          Trace.callEndpoint @"lock" hdl1 params

          hdl2 <- Trace.activateContractWallet w2 (void refundEp)
          void $ Trace.waitNSlots 100
          void $ Trace.callEndpoint @"refund" hdl2 params
    , checkPredicateOptions options "can't redeem if you can't pay"
        ( walletFundsChange w1 (token1 (-10))
          .&&. walletFundsChange w2 mempty
        )
        $ do
          -- 501 token1 is _just_ too much; we don't have enough ( our options
          -- allocate only 500 ).
          let params = mkEscrowParams startTime (token1 10) (token2 501)

          hdl1 <- Trace.activateContractWallet w1 lockEp
          Trace.callEndpoint @"lock" hdl1 params
          void $ Trace.waitNSlots 1

          hdl2 <- Trace.activateContractWallet w2 (void redeemEp)
          void $ Trace.callEndpoint @"redeem" hdl2 params
    ]

    where
        startTime = TimeSlot.scSlotZeroTime def


token1 :: Integer -> Value
token1 = Value.singleton "1111" "Token1"

token2 :: Integer -> Value
token2 = Value.singleton "2222" "Token2"

options :: CheckOptions
options =
    let initialDistribution = defaultDist & over (at w1 . _Just) ((<>) (token1 500))
                                          & over (at w2 . _Just) ((<>) (token2 500))
    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

mkEscrowParams :: POSIXTime -> Value -> Value -> EscrowParams
mkEscrowParams startTime p e =
  EscrowParams
    { payee     = pubKeyHash $ walletPubKey w1
    , paying    = p
    , expecting = e
    , deadline  = startTime + 100999
    }

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
