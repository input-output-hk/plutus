{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
-- | Turn 'UnbalancedTx' values into transactions using the
--   wallet API.
module Plutus.Contract.Wallet(
      balanceTx
    , handleTx
    , getUnspentOutput
    , WAPI.startWatching
    , WAPI.signTxAndSubmit
    ) where

import           Control.Monad               ((>=>))
import           Control.Monad.Error.Lens    (throwing)
import           Control.Monad.Freer         (Eff, Member)
import           Control.Monad.Freer.Error   (Error, throwError)
import qualified Data.Set                    as Set
import           Data.Void                   (Void)
import qualified Ledger.Ada                  as Ada
import           Ledger.Constraints          (mustPayToPubKey)
import           Ledger.Constraints.OffChain (UnbalancedTx (..), mkTx)
import           Ledger.Crypto               (pubKeyHash)
import           Ledger.Tx                   (Tx (..), TxOutRef, txInRef)
import qualified Plutus.Contract.Request     as Contract
import           Plutus.Contract.Types       (Contract)
import qualified Wallet.API                  as WAPI
import           Wallet.Effects
import           Wallet.Emulator.Error       (WalletAPIError)
import           Wallet.Types                (AsContractError (_ConstraintResolutionError, _OtherError))

{- Note [Submitting transactions from Plutus contracts]

'UnbalancedTx' is the type of transactions that meet some set of constraints
(produced by 'Ledger.Constraints.OffChain.mkTx'), but can't be submitted to
the ledger yet because they may not be balanced and they lack signatures and
fee payments. To turn an 'UnbalancedTx' value into a valid transaction that can
be submitted to the network, the contract backend needs to

* Balance it.
  If the total value of 'txInputs' + the 'txMint' field is
  greater than the total value of 'txOutputs', then one or more public key
  outputs need to be added. How many and what addresses they are is up
  to the wallet (probably configurable).
  If the total balance 'txInputs' + the 'txMint' field is less than
  the total value of 'txOutputs', then one or more public key inputs need
  to be added (and potentially some outputs for the change).

* Compute fees.
  Once the final size of the transaction is known, the fees for the transaction
  can be computed. The transaction fee needs to be paid for with additional
  inputs so I assume that this step and the previous step will be combined.

  Also note that even if the 'UnbalancedTx' that we get from the contract
  endpoint happens to be balanced already, we still need to add fees to it. So
  we can't skip the balancing & fee computation step.

  Balancing and coin selection will eventually be performed by the wallet
  backend.

* Sign it.
  The signing process needs to provide signatures for all public key
  inputs in the balanced transaction, and for all public keys in the
  'unBalancedTxRequiredSignatories' field.

-}

-- | Balance an unabalanced transaction, sign it, and submit
--   it to the chain in the context of a wallet.
handleTx ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    )
    => UnbalancedTx -> Eff effs Tx
handleTx = balanceTx >=> either throwError WAPI.signTxAndSubmit

-- | Get an unspent output belonging to the wallet.
getUnspentOutput :: AsContractError e => Contract w s e TxOutRef
getUnspentOutput = do
    ownPK <- Contract.ownPubKey
    let constraints = mustPayToPubKey (pubKeyHash ownPK) (Ada.lovelaceValueOf 1)
    utx <- either (throwing _ConstraintResolutionError) pure (mkTx @Void mempty constraints)
    tx <- Contract.balanceTx utx
    case Set.lookupMin (txInputs tx) of
        Just inp -> pure $ txInRef inp
        Nothing  -> throwing _OtherError "Balanced transaction has no inputs"
