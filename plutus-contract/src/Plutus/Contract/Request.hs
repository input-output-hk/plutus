{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Plutus.Contract.Request(
    -- * PAB requests
    -- ** Waiting
    getSlotConfig
    , awaitSlot
    , currentSlot
    , waitNSlots
    , awaitTime
    , currentTime
    , waitNMilliSeconds
    -- ** Querying the UTXO set
    , utxoAt
    -- ** Waiting for changes to the UTXO set
    , addressChangeRequest
    , nextTransactionsAt
    , fundsAtAddressGt
    , fundsAtAddressGeq
    , fundsAtAddressCondition
    , watchAddressUntilSlot
    , watchAddressUntilTime
    -- ** Tx confirmation
    , awaitTxConfirmed
    -- ** Contract instances
    , ownInstanceId
    -- ** Exposing endpoints
    , HasEndpoint
    , EndpointDescription(..)
    , Endpoint
    , endpoint
    , endpointWithMeta
    , endpointDescription
    , endpointReq
    , endpointResp
    -- ** Public keys
    , ownPubKey
    -- ** Submitting transactions
    , submitUnbalancedTx
    , submitBalancedTx
    , balanceTx
    , submitTx
    , submitTxConstraints
    , submitTxConstraintsSpending
    , submitTxConstraintsWith
    , submitTxConfirmed
    -- * Etc.
    , ContractRow
    , pabReq
    ) where

import           Control.Applicative
import           Control.Lens                (Prism', preview, review, view)
import           Control.Monad               (void)
import qualified Control.Monad.Freer.Error   as E
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Aeson                  as JSON
import qualified Data.Aeson.Types            as JSON
import           Data.Proxy                  (Proxy (..))
import           Data.Row
import qualified Data.Text                   as Text
import           Data.Text.Extras            (tshow)
import           Data.Void                   (Void)
import           GHC.Natural                 (Natural)
import           GHC.TypeLits                (Symbol, symbolVal)
import           Ledger                      (Address, DiffMilliSeconds, OnChainTx (..), POSIXTime, PubKey, Slot, Tx,
                                              TxId, TxOut (..), TxOutTx (..), Value, fromMilliSeconds, txId)
import           Ledger.AddressMap           (UtxoMap)
import           Ledger.Constraints          (TxConstraints)
import           Ledger.Constraints.OffChain (ScriptLookups, UnbalancedTx)
import qualified Ledger.Constraints.OffChain as Constraints
import           Ledger.Typed.Scripts        (TypedValidator, ValidatorTypes (..))
import qualified Ledger.Value                as V
import           Plutus.Contract.Util        (loopM)
import qualified PlutusTx

import           Plutus.Contract.Effects     (ActiveEndpoint (..), PABReq (..), PABResp (..), UtxoAtAddress (..))
import qualified Plutus.Contract.Effects     as E
import           Plutus.Contract.Schema      (Input, Output)
import           Wallet.Types                (AddressChangeRequest (..), AddressChangeResponse (..), ContractInstanceId,
                                              EndpointDescription (..), EndpointValue (..), targetSlot)

import           Plutus.Contract.Resumable
import           Plutus.Contract.Types

import           Ledger.TimeSlot             (SlotConfig)
import           Prelude                     as Haskell

-- | Constraints on the contract schema, ensuring that the labels of the schema
--   are unique.
type ContractRow s =
  ( AllUniqueLabels (Input s)
  , AllUniqueLabels (Output s)
  )

{- Send a 'PABReq' and return the appropriate 'PABResp'
-}
pabReq ::
  forall w s e a.
  ( AsContractError e
  )
  => PABReq -- ^ The request to send
  -> Prism' PABResp a -- ^ Prism for the response
  -> Contract w s e a
pabReq req prism = Contract $ do
  x <- prompt @PABResp @PABReq req
  case preview prism x of
    Just r -> pure r
    _      -> E.throwError @e $ review _ResumableError $ WrongVariantError $ "unexpected answer: " <> tshow x

-- | Wait until the slot
awaitSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Slot
    -> Contract w s e Slot
awaitSlot s = pabReq (AwaitSlotReq s) E._AwaitSlotResp

-- | Get the 'SlotConfig' which defines the length of 1 slot and the starting time.
getSlotConfig ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e SlotConfig
getSlotConfig = pabReq GetSlotConfigReq E._GetSlotConfigResp

-- | Get the current slot number
currentSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e Slot
currentSlot = pabReq CurrentSlotReq E._CurrentSlotResp

-- | Wait for a number of slots to pass
waitNSlots ::
  forall w s e.
  ( AsContractError e
  )
  => Natural
  -> Contract w s e Slot
waitNSlots n = do
  c <- currentSlot
  awaitSlot $ c + fromIntegral n

-- | Wait until the slot where the given time falls into and return latest time
-- we know has passed.
--
-- Example: if starting time is 0 and slot length is 3s, then `awaitTime 4`
-- waits until slot 2 and returns the value `POSIXTime 5`.
awaitTime ::
    forall w s e.
    ( AsContractError e
    )
    => POSIXTime
    -> Contract w s e POSIXTime
awaitTime s = pabReq (AwaitTimeReq s) E._AwaitTimeResp

-- | Get the latest time of the current slot.
--
-- Example: if slot length is 3s and current slot is 2, then `currentTime`
-- returns the value `POSIXTime 5`
currentTime ::
    forall w s e.
    ( AsContractError e
    )
    => Contract w s e POSIXTime
currentTime = pabReq CurrentTimeReq E._CurrentTimeResp

-- | Wait for a number of milliseconds starting at the ending time of the current
-- slot, and return the latest time we know has passed.
--
-- Example: if starting time is 0, slot length is 3000ms and current slot is 0, then
-- `waitNMilliSeconds 0` returns the value `POSIXTime 2000` and `waitNMilliSeconds 1000`
-- returns the value `POSIXTime 5`.
waitNMilliSeconds ::
  forall w s e.
  ( AsContractError e
  )
  => DiffMilliSeconds
  -> Contract w s e POSIXTime
waitNMilliSeconds n = do
  t <- currentTime
  awaitTime $ t + fromMilliSeconds n

-- | Get the unspent transaction outputs at an address.
utxoAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e UtxoMap
utxoAt addr = fmap utxo $ pabReq (UtxoAtReq addr) E._UtxoAtResp

-- | Wait until the target slot and get the unspent transaction outputs at an
-- address.
watchAddressUntilSlot ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Slot
    -> Contract w s e UtxoMap
watchAddressUntilSlot a slot = awaitSlot slot >> utxoAt a

-- | Wait until the target time and get the unspent transaction outputs at an
-- address.
watchAddressUntilTime ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> POSIXTime
    -> Contract w s e UtxoMap
watchAddressUntilTime a time = awaitTime time >> utxoAt a

{-| Get the transactions that modified an address in a specific slot.
-}
addressChangeRequest ::
    forall w s e.
    ( AsContractError e
    )
    => AddressChangeRequest
    -> Contract w s e AddressChangeResponse
addressChangeRequest r = do
  _ <- awaitSlot (targetSlot r)
  pabReq (AddressChangeReq r) E._AddressChangeResp

-- | Call 'addresssChangeRequest' for the address in each slot, until at least one
--   transaction is returned that modifies the address.
nextTransactionsAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> Contract w s e [OnChainTx]
nextTransactionsAt addr = do
    initial <- currentSlot
    let go :: Slot -> Contract w s ContractError (Either [OnChainTx] Slot)
        go sl = do
            let request = AddressChangeRequest{acreqSlotRangeFrom = sl, acreqSlotRangeTo = sl, acreqAddress=addr}
            _ <- awaitSlot (targetSlot request)
            txns <- acrTxns <$> addressChangeRequest request
            if null txns
                then pure $ Right (succ sl)
                else pure $ Left txns
    mapError (review _ContractError) (checkpointLoop go initial)

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt
    :: forall w s e.
       ( AsContractError e
       )
    => Address
    -> Value
    -> Contract w s e UtxoMap
fundsAtAddressGt addr vl =
    fundsAtAddressCondition (\presentVal -> presentVal `V.gt` vl) addr

fundsAtAddressCondition
    :: forall w s e.
       ( AsContractError e
       )
    => (Value -> Bool)
    -> Address
    -> Contract w s e UtxoMap
fundsAtAddressCondition condition addr = loopM go () where
    go () = do
        cur <- utxoAt addr
        sl <- currentSlot
        let presentVal = foldMap (txOutValue . txOutTxOut) cur
        if condition presentVal
            then pure (Right cur)
            else awaitSlot (sl + 1) >> pure (Left ())

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has reached or surpassed the given value.
fundsAtAddressGeq
    :: forall w s e.
       ( AsContractError e
       )
    => Address
    -> Value
    -> Contract w s e UtxoMap
fundsAtAddressGeq addr vl =
    fundsAtAddressCondition (\presentVal -> presentVal `V.geq` vl) addr

-- TODO: Configurable level of confirmation (for example, as soon as the tx is
--       included in a block, or only when it can't be rolled back anymore)
-- | Wait until a transaction is confirmed (added to the ledger).
awaitTxConfirmed :: forall w s e. (AsContractError e) => TxId -> Contract w s e ()
awaitTxConfirmed i = void $ pabReq (AwaitTxConfirmedReq i) E._AwaitTxConfirmedResp

-- | Get the 'ContractInstanceId' of this instance.
ownInstanceId :: forall w s e. (AsContractError e) => Contract w s e ContractInstanceId
ownInstanceId = pabReq OwnContractInstanceIdReq E._OwnContractInstanceIdResp

type HasEndpoint l a s =
  ( HasType l (EndpointValue a) (Input s)
  , HasType l ActiveEndpoint (Output s)
  , KnownSymbol l
  , ContractRow s
  )

type Endpoint l a = l .== (EndpointValue a, ActiveEndpoint)

endpointReq :: forall l a s.
    ( HasEndpoint l a s )
    => ActiveEndpoint
endpointReq =
    ActiveEndpoint
        { aeDescription = EndpointDescription $ symbolVal (Proxy @l)
        , aeMetadata    = Nothing
        }

endpointDesc :: forall (l :: Symbol). KnownSymbol l => EndpointDescription
endpointDesc = EndpointDescription $ symbolVal (Proxy @l)

endpointResp :: forall l a s. (HasEndpoint l a s, ToJSON a) => a -> PABResp
endpointResp =  ExposeEndpointResp (endpointDesc @l) . EndpointValue . JSON.toJSON

-- | Expose an endpoint, return the data that was entered
endpoint
  :: forall l a w s e.
     ( HasEndpoint l a s
     , AsContractError e
     , FromJSON a
     )
  => Contract w s e a
endpoint = pabReq (ExposeEndpointReq $ endpointReq @l @a @s) E._ExposeEndpointResp >>= decode . snd

decode :: forall a w s e. (FromJSON a, AsContractError e) => EndpointValue JSON.Value -> Contract w s e a
decode EndpointValue{unEndpointValue} =
    either (throwError . review _OtherError . Text.pack) pure
    $ JSON.parseEither JSON.parseJSON unEndpointValue

-- | Expose an endpoint with some metadata. Return the data that was entered.
endpointWithMeta
  :: forall l a w s e b.
     ( HasEndpoint l a s
     , AsContractError e
     , ToJSON b
     , FromJSON a
     )
  => b
  -> Contract w s e a
endpointWithMeta b = pabReq (ExposeEndpointReq s) E._ExposeEndpointResp >>= decode . snd
    where
        s = ActiveEndpoint
                { aeDescription = endpointDesc @l
                , aeMetadata    = Just $ JSON.toJSON b
                }

endpointDescription :: forall l. KnownSymbol l => Proxy l -> EndpointDescription
endpointDescription = EndpointDescription . symbolVal

-- | Get a public key belonging to the wallet that runs this contract.
--   * Any funds paid to this public key will be treated as the wallet's own
--     funds
--   * The wallet is able to sign transactions with the private key of this
--     public key, for example, if the public key is added to the
--     'requiredSignatures' field of 'Tx'.
--   * There is a 1-n relationship between wallets and public keys (although in
--     the mockchain n=1)
ownPubKey :: forall w s e. (AsContractError e) => Contract w s e PubKey
ownPubKey = pabReq OwnPublicKeyReq E._OwnPublicKeyResp

-- | Send an unbalanced transaction to be balanced and signed. Returns the ID
--    of the final transaction when the transaction was submitted. Throws an
--    error if balancing or signing failed.
submitUnbalancedTx :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e Tx
-- See Note [Injecting errors into the user's error type]
submitUnbalancedTx utx = do
  tx <- balanceTx utx
  submitBalancedTx tx

-- | Send an unbalanced transaction to be balanced. Returns the balanced transaction.
--    Throws an error if balancing failed.
balanceTx :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e Tx
-- See Note [Injecting errors into the user's error type]
balanceTx t =
  let req = pabReq (BalanceTxReq t) E._BalanceTxResp in
  req >>= either (throwError . review _WalletError) pure . view E.balanceTxResponse

-- | Send an balanced transaction to be signed. Returns the ID
--    of the final transaction when the transaction was submitted. Throws an
--    error if signing failed.
submitBalancedTx :: forall w s e. (AsContractError e) => Tx -> Contract w s e Tx
-- See Note [Injecting errors into the user's error type]
submitBalancedTx t =
  let req = pabReq (WriteBalancedTxReq t) E._WriteBalancedTxResp in
  req >>= either (throwError . review _WalletError) pure . view E.writeBalancedTxResponse

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. The constraints do not refer to any typed script inputs or
--   outputs.
submitTx :: forall w s e.
  ( AsContractError e
  )
  => TxConstraints Void Void
  -> Contract w s e Tx
submitTx = submitTxConstraintsWith @Void mempty

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. Using the current outputs at the contract address and the
--   contract's own public key to solve the constraints.
submitTxConstraints
  :: forall a w s e.
  ( PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => TypedValidator a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraints inst = submitTxConstraintsWith (Constraints.typedValidatorLookups inst)

-- | Build a transaction that satisfies the constraints using the UTXO map
--   to resolve any input constraints (see 'Ledger.Constraints.TxConstraints.InputConstraint')
submitTxConstraintsSpending
  :: forall a w s e.
  ( PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => TypedValidator a
  -> UtxoMap
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraintsSpending inst utxo =
  let lookups = Constraints.typedValidatorLookups inst <> Constraints.unspentOutputs utxo
  in submitTxConstraintsWith lookups

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. Using the given constraints.
submitTxConstraintsWith
  :: forall a w s e.
  ( PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => ScriptLookups a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraintsWith sl constraints = do
  tx <- either (throwError . review _ConstraintResolutionError) pure (Constraints.mkTx sl constraints)
  submitUnbalancedTx tx

-- | A version of 'submitTx' that waits until the transaction has been
--   confirmed on the ledger before returning.
submitTxConfirmed :: forall w s e. (AsContractError e) => UnbalancedTx -> Contract w s e ()
submitTxConfirmed t = submitUnbalancedTx t >>= awaitTxConfirmed . txId

