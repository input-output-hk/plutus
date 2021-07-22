{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TypeApplications #-}
{-

A 'Simulator' for the test contracts

-}
module Plutus.PAB.Simulator.Test(runSimulation) where

import           Control.Monad.Freer                      (interpret)
import           Data.Default                             (Default (def))
import           Plutus.PAB.Core                          (EffectHandlers)
import           Plutus.PAB.Effects.Contract.Builtin      (Builtin)
import           Plutus.PAB.Effects.Contract.ContractTest (TestContracts (..), handleContractTest)
import           Plutus.PAB.Simulator                     (Simulation, SimulatorContractHandler, SimulatorState,
                                                           mkSimulatorHandlers, runSimulationWith)
import           Plutus.PAB.Types                         (PABError)

-- | 'EffectHandlers' for running the PAB as a simulator (no connectivity to
--   out-of-process services such as wallet backend, node, etc.)
simulatorHandlers :: EffectHandlers (Builtin TestContracts) (SimulatorState (Builtin TestContracts))
simulatorHandlers = mkSimulatorHandlers @(Builtin TestContracts) def def [GameStateMachine, Currency, AtomicSwap] handler where
    handler :: SimulatorContractHandler (Builtin TestContracts)
    handler = interpret handleContractTest

-- | Run the PAB simulator with the test contracts
runSimulation :: Simulation (Builtin TestContracts) a -> IO (Either PABError a)
runSimulation = runSimulationWith @(Builtin TestContracts) simulatorHandlers
