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

module ContractExample(
    ExampleContracts(..)
    , handlers
    ) where

import           Control.Monad.Freer
import           Data.Aeson                                (FromJSON, ToJSON)
import           Data.Default                              (Default (def))
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                              (Generic)

import qualified ContractExample.AtomicSwap                as Contracts.AtomicSwap
import qualified ContractExample.PayToWallet               as Contracts.PayToWallet
import qualified ContractExample.WaitForTx                 as Contracts.WaitForTx
import           Data.Data                                 (Proxy (Proxy))
import           Data.Row
import           Language.PureScript.Bridge                (equal, genericShow, mkSumType)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Playground.Types                          (FunctionSchema)
import qualified Plutus.Contracts.Currency                 as Contracts.Currency
import qualified Plutus.Contracts.GameStateMachine         as Contracts.GameStateMachine
import qualified Plutus.Contracts.Prism.Mirror             as Contracts.Prism
import qualified Plutus.Contracts.Prism.Unlock             as Contracts.Prism
import           Plutus.Contracts.Uniswap                  (Uniswap)
import qualified Plutus.Contracts.Uniswap                  as Contracts.Uniswap
import           Plutus.Contracts.Uniswap.Types            (Coin, U)
import           Plutus.PAB.Effects.Contract.Builtin       (Builtin, BuiltinHandler (..), HasDefinitions (..),
                                                            SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin       as Builtin
import           Plutus.PAB.Run.PSGenerator                (HasPSTypes (..))
import           Plutus.PAB.Simulator                      (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                      as Simulator
import           Schema                                    (FormSchema)

data ExampleContracts = UniswapInit
                      | UniswapOwner
                      | UniswapUser Contracts.Uniswap.Uniswap
                      | GameStateMachine
                      | PayToWallet
                      | AtomicSwap
                      | Currency
                      | PrismMirror
                      | PrismUnlockExchange
                      | PrismUnlockSto
                      | WaitForTx
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ExampleContracts where
    pretty = viaShow

instance HasPSTypes ExampleContracts where
    psTypes p =
        [ (equal <*> (genericShow <*> mkSumType)) p
        -- These types come from the Uniswap contract and need to be available in PS
        , (equal <*> (genericShow <*> mkSumType)) (Proxy @Uniswap)
        , (equal <*> (genericShow <*> mkSumType)) (Proxy @(Coin A))
        , (equal <*> (genericShow <*> mkSumType)) (Proxy @U)
        ]

instance HasDefinitions ExampleContracts where
    getDefinitions = [ UniswapInit
                     , UniswapOwner
                     , GameStateMachine
                     , PayToWallet
                     , AtomicSwap
                     , Currency
                     , PrismMirror
                     , PrismUnlockExchange
                     , PrismUnlockSto
                     , WaitForTx
                     ]
    getContract = getExampleContracts
    getSchema = getExampleContractsSchema

getExampleContractsSchema :: ExampleContracts -> [FunctionSchema FormSchema]
getExampleContractsSchema = \case
    UniswapInit         -> Builtin.endpointsToSchemas @Empty
    UniswapUser _       -> Builtin.endpointsToSchemas @Contracts.Uniswap.UniswapUserSchema
    UniswapOwner        -> Builtin.endpointsToSchemas @Contracts.Uniswap.UniswapOwnerSchema
    GameStateMachine    -> Builtin.endpointsToSchemas @Contracts.GameStateMachine.GameStateMachineSchema
    PayToWallet         -> Builtin.endpointsToSchemas @Contracts.PayToWallet.PayToWalletSchema
    AtomicSwap          -> Builtin.endpointsToSchemas @Contracts.AtomicSwap.AtomicSwapSchema
    Currency            -> Builtin.endpointsToSchemas @Contracts.Currency.CurrencySchema
    PrismMirror         -> Builtin.endpointsToSchemas @Contracts.Prism.MirrorSchema
    PrismUnlockExchange -> Builtin.endpointsToSchemas @Contracts.Prism.UnlockExchangeSchema
    PrismUnlockSto      -> Builtin.endpointsToSchemas @Contracts.Prism.STOSubscriberSchema
    WaitForTx           -> Builtin.endpointsToSchemas @Contracts.WaitForTx.WaitForTxSchema

getExampleContracts :: ExampleContracts -> SomeBuiltin
getExampleContracts = \case
    UniswapInit         -> SomeBuiltin Contracts.Uniswap.setupTokens
    UniswapUser us      -> SomeBuiltin $ Contracts.Uniswap.userEndpoints us
    UniswapOwner        -> SomeBuiltin Contracts.Uniswap.ownerEndpoint
    GameStateMachine    -> SomeBuiltin Contracts.GameStateMachine.contract
    PayToWallet         -> SomeBuiltin Contracts.PayToWallet.payToWallet
    AtomicSwap          -> SomeBuiltin Contracts.AtomicSwap.atomicSwap
    Currency            -> SomeBuiltin Contracts.Currency.mintCurrency
    PrismMirror         -> SomeBuiltin (Contracts.Prism.mirror @Contracts.Prism.MirrorSchema @())
    PrismUnlockExchange -> SomeBuiltin (Contracts.Prism.unlockExchange @() @Contracts.Prism.UnlockExchangeSchema)
    PrismUnlockSto      -> SomeBuiltin (Contracts.Prism.subscribeSTO @() @Contracts.Prism.STOSubscriberSchema)
    WaitForTx           -> SomeBuiltin Contracts.WaitForTx.waitForTx

handlers :: SimulatorEffectHandlers (Builtin ExampleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin ExampleContracts) def
    $ interpret (contractHandler Builtin.handleBuiltin)
