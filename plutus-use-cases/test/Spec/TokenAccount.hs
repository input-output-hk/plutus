{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.TokenAccount(tests, assertAccountBalance, tokenAccountTrace) where

import           Test.Tasty

import           Control.Monad                 (void)
import           Control.Monad.Freer           (run)
import           Control.Monad.Freer.Error     (runError)
import           Data.Default                  (Default (..))
import qualified Ledger
import qualified Ledger.Ada                    as Ada
import           Ledger.Value                  (TokenName, Value)
import           Plutus.Contract               (Contract)
import           Plutus.Contract.Test
import qualified Streaming.Prelude             as S
import           Wallet.Emulator.Stream        (foldEmulatorStreamM, takeUntilSlot)

import           Plutus.Contracts.TokenAccount (Account (..), TokenAccountError, TokenAccountSchema,
                                                tokenAccountContract)
import qualified Plutus.Contracts.TokenAccount as Accounts
import qualified Plutus.Trace.Emulator         as Trace
import qualified Wallet.Emulator.Folds         as Folds

tests :: TestTree
tests = testGroup "token account"
    [ checkPredicate "Create a token account"
        (assertNoFailedTransactions
        .&&. assertNotDone contract (Trace.walletInstanceTag w1) "contract should not have any errors"
        .&&. walletFundsChange w1 theToken)
        $ do
            hdl <- Trace.activateContractWallet w1 contract
            Trace.callEndpoint @"new-account" hdl (tokenName, Ledger.pubKeyHash $ walletPubKey w1)
            void $ Trace.waitNSlots 2

    , checkPredicate "Pay into the account"
        (assertNoFailedTransactions
        .&&. assertNotDone contract (Trace.walletInstanceTag w1) "contract should not have any errors"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10) <> theToken))
        $ do
            hdl <- Trace.activateContractWallet w1 contract
            Trace.callEndpoint @"new-account" hdl (tokenName, Ledger.pubKeyHash $ walletPubKey w1)
            _ <- Trace.waitNSlots 3
            Trace.callEndpoint @"pay" hdl (account, Ada.lovelaceValueOf 10)
            void $ Trace.waitNSlots 1

    , checkPredicate "Transfer & redeem all funds"
        (assertNoFailedTransactions
        .&&. assertNotDone contract (Trace.walletInstanceTag w1) "contract should not have any errors"
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-10))
        .&&. walletFundsChange w2 (theToken <> Ada.lovelaceValueOf 10)
        )
        tokenAccountTrace

    ]

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tokenName :: TokenName
tokenName = "test token"

contract :: Contract () TokenAccountSchema TokenAccountError ()
contract = tokenAccountContract

account :: Account
account =
    let con = Accounts.newAccount @() @TokenAccountSchema @TokenAccountError tokenName (Ledger.pubKeyHash $ walletPubKey w1)
        fld = Folds.instanceOutcome @() con (Trace.walletInstanceTag w1)
        trace = Trace.activateContractWallet @() w1 (void con) >> Trace.waitNSlots 2
        getOutcome (Done a) = a
        getOutcome _        = error "not finished"
    in
    either (error . show) (getOutcome . S.fst')
        $ run
        $ runError @Folds.EmulatorFoldErr
        $ foldEmulatorStreamM fld
        $ takeUntilSlot 10
        $ Trace.runEmulatorStream def trace

theToken :: Value
theToken = Accounts.accountToken account

-- | Check that the balance of the given account satisfies a predicate.
assertAccountBalance :: Account -> (Value -> Bool) -> TracePredicate
assertAccountBalance acc = valueAtAddress (Accounts.address acc)

-- | Create a new token account for wallet 1, pay 100 lovelace to the token
--   account contract, transfer the token to wallet 2, then use the token
--   to take out the funds.
tokenAccountTrace :: Trace.EmulatorTrace ()
tokenAccountTrace = do
    hdl <- Trace.activateContractWallet w1 contract
    hdl2 <- Trace.activateContractWallet w2 contract
    Trace.callEndpoint @"new-account" hdl (tokenName, Ledger.pubKeyHash $ walletPubKey w1)
    _ <- Trace.waitNSlots 3
    Trace.callEndpoint @"pay" hdl (account, Ada.lovelaceValueOf 10)
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 theToken
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"redeem" hdl2 (account, Ledger.pubKeyHash $ walletPubKey w2)
    void $ Trace.waitNSlots 1
