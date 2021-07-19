{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-

Interface to beam ecosystem used by the PAB to store contracts.

-}
module Plutus.PAB.Db.Beam
  where

import           Cardano.BM.Trace                    (Trace)
import           Control.Monad.Freer                 (Eff, interpret, reinterpret, runM, subsume)
import           Control.Monad.Freer.Delay           (DelayEffect, handleDelayEffect)
import           Control.Monad.Freer.Error           (runError)
import           Control.Monad.Freer.Extras          (LogMsg, mapLog)
import qualified Control.Monad.Freer.Extras.Modify   as Modify
import           Control.Monad.Freer.Reader          (runReader)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Database.SQLite.Simple              (Connection)
import           Plutus.PAB.Db.Beam.ContractStore    (handleContractStore)
import           Plutus.PAB.Effects.Contract         (ContractStore)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions)
import           Plutus.PAB.Effects.DbStore
import           Plutus.PAB.Monitoring.Monitoring    (handleLogMsgTrace)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABLogMsg (..), PABMultiAgentMsg (..))
import           Plutus.PAB.Types                    (PABError)


-- | Run the ContractStore and ContractDefinitionStore effects on the
--   SQLite database.
runBeamStoreAction ::
    forall a b.
    ( ToJSON a
    , FromJSON a
    , HasDefinitions a
    )
    => Connection
    -> Trace IO (PABLogMsg (Builtin a))
    -> Eff '[ContractStore (Builtin a), LogMsg (PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
    -> IO (Either PABError b)
runBeamStoreAction connection trace =
    runM
    . runError
    . runReader connection
    . interpret (handleDbStore trace)
    . subsume @IO
    . handleDelayEffect
    . interpret (handleLogMsgTrace trace)
    . reinterpret (mapLog SMultiAgent)
    . interpret handleContractStore
    . Modify.raiseEnd
