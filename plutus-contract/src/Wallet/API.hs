{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

{-|

Mock wallet implementation

-}
module Wallet.API(
    WalletEffect,
    submitTxn,
    ownPubKey,
    balanceTx,
    NodeClientEffect,
    publishTx,
    getClientSlot,
    getClientSlotConfig,
    ChainIndexEffect,
    startWatching,
    watchedAddresses,
    PubKey(..),
    signTxAndSubmit,
    signTxAndSubmit_,
    payToPublicKey,
    payToPublicKey_,
    -- * Slot ranges
    Interval(..),
    Slot,
    SlotRange,
    width,
    defaultSlotRange,
    interval,
    singleton,
    isEmpty,
    always,
    member,
    before,
    after,
    contains,
    -- * Error handling
    WalletAPIError(..),
    throwInsufficientFundsError,
    throwOtherError,
    ) where

import           Control.Monad               (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error   (Error, throwError)
import           Ledger                      hiding (inputs, out, value)
import           Ledger.Constraints.OffChain (UnbalancedTx (..), emptyUnbalancedTx)
import           Wallet.Effects
import           Wallet.Emulator.Error

import           Prelude                     hiding (Ordering (..))

-- | Transfer some funds to an address locked by a public key, returning the
--   transaction that was submitted.
payToPublicKey ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    )
    => SlotRange -> Value -> PubKey -> Eff effs Tx
payToPublicKey range v pk = do
    let tx = mempty{txOutputs = [pubKeyTxOut v pk], txValidRange = range}
    balancedTx <- balanceTx emptyUnbalancedTx{unBalancedTxTx = tx}
    either throwError signTxAndSubmit balancedTx

-- | Transfer some funds to an address locked by a public key.
payToPublicKey_ ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    )
    => SlotRange -> Value -> PubKey -> Eff effs ()
payToPublicKey_ r v = void . payToPublicKey r v

-- | Add the wallet's signature to the transaction and submit it. Returns
--   the transaction with the wallet's signature.
signTxAndSubmit ::
    ( Member WalletEffect effs
    )
    => Tx -> Eff effs Tx
signTxAndSubmit t = do
    tx' <- walletAddSignature t
    submitTxn tx'
    pure tx'

-- | A version of 'signTxAndSubmit' that discards the result.
signTxAndSubmit_ ::
    ( Member WalletEffect effs
    )
    => Tx -> Eff effs ()
signTxAndSubmit_ = void . signTxAndSubmit

-- | The default slot validity range for transactions.
defaultSlotRange :: SlotRange
defaultSlotRange = always
