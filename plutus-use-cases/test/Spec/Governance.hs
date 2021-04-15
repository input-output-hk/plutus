{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-strictness  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Spec.Governance(tests, doVoting) where

import           Test.Tasty                  (TestTree, testGroup)
import qualified Test.Tasty.HUnit            as HUnit

import           Control.Lens                (view)
import           Data.Foldable               (traverse_)

import qualified Ledger
import qualified Ledger.Typed.Scripts        as Scripts
import qualified Wallet.Emulator             as EM

import           Plutus.Contract.Test
-- import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Governance as Gov
import           Plutus.Trace.Emulator       (EmulatorTrace)
import qualified Plutus.Trace.Emulator       as Trace
import qualified PlutusTx                    as PlutusTx
import           PlutusTx.Prelude            (ByteString)

tests :: TestTree
tests =
    testGroup "governance tests"
    [ checkPredicate "vote all in favor, 2 rounds - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.scriptAddress $ Gov.scriptInstance params) ((== lawv3) . Gov.law))
        (doVoting 10 0 2)

    , checkPredicate "vote 60/40, accepted - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.scriptAddress $ Gov.scriptInstance params) ((== lawv2) . Gov.law))
        (doVoting 6 4 1)

    , checkPredicate "vote 50/50, rejected - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.scriptAddress $ Gov.scriptInstance params) ((== lawv1) . Gov.law))
        (doVoting 5 5 1)

    , goldenPir "test/Spec/governance.pir" $$(PlutusTx.compile [|| Gov.mkValidator ||])
    , HUnit.testCase "script size is reasonable" (reasonable (Scripts.validatorScript $ Gov.scriptInstance params) 20000)
    ]

numberOfHolders :: Integer
numberOfHolders = 10

baseName :: Ledger.TokenName
baseName = "TestLawToken"

-- | A governance contract that requires 6 votes out of 10
params :: Gov.Params
params = Gov.Params holders 6 baseName where
    holders = Ledger.pubKeyHash . EM.walletPubKey . EM.Wallet <$> [1..numberOfHolders]

lawv1, lawv2, lawv3 :: ByteString
lawv1 = "Law v1"
lawv2 = "Law v2"
lawv3 = "Law v3"

doVoting :: Int -> Int -> Integer -> EmulatorTrace ()
doVoting ayes nays rounds = do
    let activate w = (Gov.mkTokenName baseName w,) <$> Trace.activateContractWallet (EM.Wallet w) (Gov.contract @Gov.GovError params)
    namesAndHandles <- traverse activate [1..numberOfHolders]
    let (_, handle1) = namesAndHandles !! 0
    let (token2, handle2) = namesAndHandles !! 1
    _ <- Trace.callEndpoint @"new-law" handle1 lawv1
    _ <- Trace.waitNSlots 10
    let votingRound (_, law) = do
            now <- view Trace.currentSlot <$> Trace.chainState
            Trace.callEndpoint @"propose-change" handle2 Gov.Proposal{ Gov.newLaw = law, Gov.votingDeadline = now + 20, Gov.tokenName = token2 }
            _ <- Trace.waitNSlots 1
            traverse_ (\(nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (nm, True)  >> Trace.waitNSlots 1) (take ayes namesAndHandles)
            traverse_ (\(nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (nm, False) >> Trace.waitNSlots 1) (take nays $ drop ayes namesAndHandles)
            Trace.waitNSlots 15

    traverse_ votingRound (zip [1..rounds] (cycle [lawv2, lawv3]))
