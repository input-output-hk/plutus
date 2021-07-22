{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-

"inline" contracts from plutus-use-cases for testing

-}
module Plutus.PAB.Effects.Contract.ContractTest(
    TestContracts(..)
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Bifunctor                      (Bifunctor (..))
import           Data.Row
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                        (Generic)

import qualified ContractExample.AtomicSwap          as Contracts.AtomicSwap
import qualified ContractExample.PayToWallet         as Contracts.PayToWallet
import           Data.Text.Extras                    (tshow)
import           Playground.Types                    (FunctionSchema)
import qualified Plutus.Contracts.Currency           as Contracts.Currency
import qualified Plutus.Contracts.GameStateMachine   as Contracts.GameStateMachine
import qualified Plutus.Contracts.PingPong           as Contracts.PingPong
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Types                    (PABError (..))
import           Schema                              (FormSchema)

data TestContracts = GameStateMachine | Currency | AtomicSwap | PayToWallet | PingPong
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TestContracts where
    pretty = viaShow

instance HasDefinitions TestContracts where
    getDefinitions = [ GameStateMachine, Currency, AtomicSwap, PayToWallet, PingPong ]
    getContract = getTestContracts
    getSchema = getTestContractsSchema

getTestContractsSchema :: TestContracts -> [FunctionSchema FormSchema]
getTestContractsSchema = \case
    GameStateMachine -> Builtin.endpointsToSchemas @Contracts.GameStateMachine.GameStateMachineSchema
    Currency         -> Builtin.endpointsToSchemas @Contracts.Currency.CurrencySchema
    AtomicSwap       -> Builtin.endpointsToSchemas @Contracts.AtomicSwap.AtomicSwapSchema
    PayToWallet      -> Builtin.endpointsToSchemas @Contracts.PayToWallet.PayToWalletSchema
    PingPong         -> Builtin.endpointsToSchemas @Contracts.PingPong.PingPongSchema

getTestContracts :: TestContracts -> SomeBuiltin
getTestContracts = \case
    GameStateMachine -> SomeBuiltin game
    Currency         -> SomeBuiltin currency
    AtomicSwap       -> SomeBuiltin swp
    PayToWallet      -> SomeBuiltin payToWallet
    PingPong         -> SomeBuiltin pingPong
    where
        game = Contracts.GameStateMachine.contract
        currency = Contracts.Currency.mintCurrency
        swp = first tshow Contracts.AtomicSwap.atomicSwap
        payToWallet = Contracts.PayToWallet.payToWallet
        pingPong = Contracts.PingPong.combined
