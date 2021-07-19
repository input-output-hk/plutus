{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
-- | A state machine with two states and two roles that take turns.
module Plutus.Contracts.PingPong(
    PingPongState(..),
    Input(..),
    PingPongError(..),
    PingPongSchema,
    runPing,
    runPong,
    initialise,
    runStop,
    runWaitForUpdate,
    combined
    ) where

import           Control.Lens
import           Control.Monad                (forever, void)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           GHC.Generics                 (Generic)
import qualified Ledger.Ada                   as Ada
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check)

import           Plutus.Contract
import           Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified Prelude                      as Haskell

data PingPongState = Pinged | Ponged | Stopped
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq PingPongState where
    Pinged == Pinged = True
    Ponged == Ponged = True
    _ == _           = False

data Input = Ping | Pong | Stop
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type PingPongSchema =
        Endpoint "initialise" ()
        .\/ Endpoint "ping" ()
        .\/ Endpoint "pong" ()
        .\/ Endpoint "stop" () -- Transition the state machine instance to the final state
        .\/ Endpoint "wait" () -- Wait for a change to the on-chain state of the machine

data PingPongError =
    PingPongContractError ContractError
    | PingPongSMError SM.SMContractError
    | StoppedUnexpectedly
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PingPongError

instance AsSMContractError PingPongError where
    _SMContractError = _PingPongSMError

instance AsContractError PingPongError where
    _ContractError = _PingPongContractError

{-# INLINABLE transition #-}
transition :: State PingPongState -> Input -> Maybe (TxConstraints Void Void, State PingPongState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (_,      Stop) -> Just (mempty, State{stateData=Stopped, stateValue=mempty})
    (Pinged, Pong) -> Just (mempty, State{stateData=Ponged, stateValue})
    (Ponged, Ping) -> Just (mempty, State{stateData=Pinged, stateValue})
    _              -> Nothing

{-# INLINABLE machine #-}
machine :: SM.StateMachine PingPongState Input
machine = SM.mkStateMachine Nothing transition isFinal where
    isFinal Stopped = True
    isFinal _       = False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType (SM.StateMachine PingPongState Input)
mkValidator = SM.mkValidator machine

typedValidator :: Scripts.TypedValidator (SM.StateMachine PingPongState Input)
typedValidator = Scripts.mkTypedValidator @(SM.StateMachine PingPongState Input)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PingPongState @Input

machineInstance :: SM.StateMachineInstance PingPongState Input
machineInstance = SM.StateMachineInstance machine typedValidator

client :: SM.StateMachineClient PingPongState Input
client = SM.mkStateMachineClient machineInstance

initialise :: forall w. Contract w PingPongSchema PingPongError (Waited ())
initialise = endpoint @"initialise" $ \() -> void $ SM.runInitialise client Pinged (Ada.lovelaceValueOf 1)

run ::
    forall w.
    PingPongState
    -> Contract w PingPongSchema PingPongError (Waited ())
    -> Contract w PingPongSchema PingPongError (Waited ())
run expectedState action = do
    let extractState = tyTxOutData . fst
        go Nothing = throwError StoppedUnexpectedly
        go (Just currentState)
            | extractState currentState == expectedState = action
            | otherwise = runWaitForUpdate >>= go . getWaited
    maybeState <- SM.getOnChainState client
    let datum = fmap fst maybeState
    go datum

runPing :: forall w. Contract w PingPongSchema PingPongError (Waited ())
runPing = run Ponged ping

ping :: forall w. Contract w PingPongSchema PingPongError (Waited ())
ping = endpoint @"ping" $ \() -> void (SM.runStep client Ping)

runPong :: forall w. Contract w PingPongSchema PingPongError (Waited ())
runPong = run Pinged pong

pong :: forall w. Contract w PingPongSchema PingPongError (Waited ())
pong = endpoint @"pong" $ \() -> void (SM.runStep client Pong)

runStop :: forall w. Contract w PingPongSchema PingPongError (Waited ())
runStop = endpoint @"stop" $ \() -> void (SM.runStep client Stop)

runWaitForUpdate :: forall w. Contract w PingPongSchema PingPongError (Waited (Maybe (OnChainState PingPongState Input)))
runWaitForUpdate = SM.waitForUpdate client

combined :: Contract (Last PingPongState) PingPongSchema PingPongError ()
combined = forever (initialise `select` ping `select` pong `select` runStop `select` wait) where
    wait = endpoint @"wait" $ \() -> do
        logInfo @Haskell.String "runWaitForUpdate"
        newState <- getWaited <$> runWaitForUpdate
        case newState of
            Nothing -> logWarn @Haskell.String "runWaitForUpdate: Nothing"
            Just (TypedScriptTxOut{tyTxOutData=s}, _) -> do
                logInfo $ "new state: " <> Haskell.show s
                tell (Last $ Just s)

PlutusTx.unstableMakeIsData ''PingPongState
PlutusTx.makeLift ''PingPongState
PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input
