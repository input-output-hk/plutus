{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Spec.PingPong(tests, pingPongTrace, twoPartiesTrace) where

import           Control.Monad                (void)
import           Data.Maybe                   (isNothing)
import           Plutus.Contract
import           Plutus.Contract.Test

import           Plutus.Contract.StateMachine (OnChainState)
import           Plutus.Contracts.PingPong    (Input, PingPongError, PingPongSchema, PingPongState)
import qualified Plutus.Contracts.PingPong    as PingPong
import qualified Plutus.Trace.Emulator        as Trace

import           Test.Tasty

theContract :: Contract () PingPongSchema PingPongError (Waited ())
theContract = do
    _ <- PingPong.initialise
    PingPong.runPong
    PingPong.runPing
    PingPong.runPong

twoParties :: Contract () PingPongSchema PingPongError (Maybe (OnChainState PingPongState Input))
twoParties =
    -- one party calls "initialise"
    -- the other party calls "stop"
    -- then the first party will learn that the instance has
    -- terminated when 'runWaitForUpdate' returns 'Nothing'.
    let p1 = PingPong.initialise >> PingPong.runWaitForUpdate
        p2 = PingPong.runStop
    in getWaited <$> (p1 `select` fmap (fmap (const Nothing)) p2)

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tests :: TestTree
tests = testGroup "pingpong"
    [ checkPredicate "activate endpoints"
        (endpointAvailable @"pong" theContract (Trace.walletInstanceTag w1))
        pingPongTrace

    , checkPredicate "Stop the contract"
        (assertDone twoParties (Trace.walletInstanceTag w1) isNothing "W1"
        .&&. assertDone twoParties (Trace.walletInstanceTag w2) isNothing "W2"
        )
        twoPartiesTrace
    ]

-- | Initialse, then call the ping and pong endpoints.
pingPongTrace :: Trace.EmulatorTrace ()
pingPongTrace = do
    hdl <- Trace.activateContractWallet w1 theContract
    Trace.callEndpoint @"initialise" hdl ()
    _ <- Trace.waitNSlots 2
    Trace.callEndpoint @"pong" hdl ()
    _ <- Trace.waitNSlots 2
    Trace.callEndpoint @"ping" hdl ()
    void $ Trace.waitNSlots 2

-- | Call 'initialise' on wallet 1, then call 'stop' in wallet 2.
twoPartiesTrace :: Trace.EmulatorTrace ()
twoPartiesTrace = do
    hdl1 <- Trace.activateContractWallet w1 (void twoParties)
    hdl2 <- Trace.activateContractWallet w2 (void twoParties)
    Trace.callEndpoint @"initialise" hdl1 ()
    _ <- Trace.waitNSlots 2
    Trace.callEndpoint @"stop" hdl2 ()
    void $ Trace.waitNSlots 2
