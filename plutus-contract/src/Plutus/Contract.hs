{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
module Plutus.Contract(
      Contract(..)
    , ContractError(..)
    , AsContractError(..)
    , both
    , selectEither
    , select
    , (>>)
    , throwError
    , handleError
    , mapError
    , runError
    -- * Dealing with time
    , Request.getSlotConfig
    , Request.awaitSlot
    , Request.currentSlot
    , Request.waitNSlots
    , Request.awaitTime
    , Request.currentTime
    , Request.waitNMilliSeconds
    -- * Endpoints
    , Request.HasEndpoint
    , Request.EndpointDescription(..)
    , Request.Endpoint
    , Request.endpoint
    , Request.endpointWithMeta
    , Schema.EmptySchema
    -- * Blockchain events
    , Wallet.Types.AddressChangeRequest(..)
    , Wallet.Types.AddressChangeResponse(..)
    , Request.addressChangeRequest
    , Request.nextTransactionsAt
    , Request.watchAddressUntilSlot
    , Request.watchAddressUntilTime
    , Request.fundsAtAddressGt
    , Request.fundsAtAddressGeq
    -- * UTXO set
    , UtxoMap
    , Request.utxoAt
    -- * Wallet's own public key
    , Request.ownPubKey
    -- * Contract instance Id
    , Wallet.Types.ContractInstanceId
    , Request.ownInstanceId
    -- * Notifications
    , tell
    -- * Transactions
    , WalletAPIError
    , Request.submitTx
    , Request.submitTxConfirmed
    , Request.submitTxConstraints
    , Request.submitTxConstraintsSpending
    , Request.submitTxConstraintsWith
    , Request.submitUnbalancedTx
    , Request.submitBalancedTx
    , Request.balanceTx
    -- ** Creating transactions
    , module Tx
    -- ** Tx confirmation
    , Request.awaitTxConfirmed
    -- * Checkpoints
    , checkpoint
    , checkpointLoop
    , AsCheckpointError(..)
    , CheckpointError(..)
    -- * Logging
    , logDebug
    , logInfo
    , logWarn
    , logError
    -- * Row-related things
    , HasType
    , ContractRow
    , type (.\/)
    , type Empty
    ) where

import           Data.Aeson                     (ToJSON (toJSON))
import           Data.Row

import           Plutus.Contract.Request        (ContractRow)
import qualified Plutus.Contract.Request        as Request
import qualified Plutus.Contract.Schema         as Schema
import           Plutus.Contract.Typed.Tx       as Tx
import           Plutus.Contract.Types          (AsCheckpointError (..), AsContractError (..), CheckpointError (..),
                                                 Contract (..), ContractError (..), checkpoint, checkpointLoop,
                                                 handleError, mapError, runError, select, selectEither, throwError)

import qualified Control.Monad.Freer.Extras.Log as L
import qualified Control.Monad.Freer.Writer     as W
import           Ledger.AddressMap              (UtxoMap)
import           Prelude
import           Wallet.API                     (WalletAPIError)
import qualified Wallet.Types

-- | Execute both contracts in any order
both :: Contract w s e a -> Contract w s e b -> Contract w s e (a, b)
both a b =
  let swap (b_, a_) = (a_, b_) in
  ((,) <$> a <*> b) `select` (fmap swap ((,) <$> b <*> a))

-- | Log a message at the 'Debug' level
logDebug :: ToJSON a => a -> Contract w s e ()
logDebug = Contract . L.logDebug . toJSON

-- | Log a message at the 'Info' level
logInfo :: ToJSON a => a -> Contract w s e ()
logInfo = Contract . L.logInfo . toJSON

-- | Log a message at the 'Warning' level
logWarn :: ToJSON a => a -> Contract w s e ()
logWarn = Contract . L.logWarn . toJSON

-- | Log a message at the 'Error' level
logError :: ToJSON a => a -> Contract w s e ()
logError = Contract . L.logError . toJSON

-- | Update the contract's accumulating state @w@
tell :: w -> Contract w s e ()
tell = Contract . W.tell
