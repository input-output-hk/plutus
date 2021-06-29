{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-

A handler for the 'ContractStore'  effect that stores everything in a TVar.

-}
module Plutus.PAB.Db.Memory.ContractStore(
    InMemContractInstanceState(..)
    , handleContractStore
    , InMemInstances
    , initialInMemInstances
    ) where

import           Control.Concurrent.STM      (TVar)
import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (_Just, at, makeLensesFor, preview, set)
import           Control.Monad.Freer         (Eff, LastMember, Member, type (~>))
import           Control.Monad.Freer.Error   (Error, throwError)
import           Control.Monad.Freer.Reader  (Reader, ask)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Map                    (Map)
import           Plutus.PAB.Effects.Contract (ContractStore)
import qualified Plutus.PAB.Effects.Contract as Contract
import           Plutus.PAB.Types            (PABError (..))
import           Plutus.PAB.Webserver.Types  (ContractActivationArgs)
import           Wallet.Types                (ContractInstanceId)

-- | The current state of a contract instance
data InMemContractInstanceState t =
    InMemContractInstanceState
        { _contractDef   :: ContractActivationArgs (Contract.ContractDef t)
        , _contractState :: Contract.State t
        }

makeLensesFor [("_contractState", "contractState")] ''InMemContractInstanceState

newtype InMemInstances t = InMemInstances { unInMemInstances :: TVar (Map ContractInstanceId (InMemContractInstanceState t)) }

initialInMemInstances :: forall t. IO (InMemInstances t)
initialInMemInstances = InMemInstances <$> STM.newTVarIO mempty

-- | Handle the 'ContractStore' effect by writing the state to the
--   TVar in 'SimulatorState'
handleContractStore ::
    forall t effs.
    ( LastMember IO effs
    , Member (Reader (InMemInstances t)) effs
    , Member (Error PABError) effs
    )
    => ContractStore t
    ~> Eff effs
handleContractStore = \case
    Contract.PutState definition instanceId state -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        liftIO $ STM.atomically $ do
            let instState = InMemContractInstanceState{_contractDef = definition, _contractState = state}
            STM.modifyTVar instancesTVar (set (at instanceId) (Just instState))
    Contract.GetState instanceId -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        result <- preview (at instanceId . _Just . contractState) <$> liftIO (STM.readTVarIO instancesTVar)
        case result of
            Just s  -> pure s
            Nothing -> throwError (ContractInstanceNotFound instanceId)
    Contract.GetActiveContracts -> do
        instancesTVar <- unInMemInstances <$> ask @(InMemInstances t)
        fmap _contractDef <$> liftIO (STM.readTVarIO instancesTVar)
    Contract.PutStartInstance{} -> pure ()
    Contract.PutStopInstance{} -> pure ()
