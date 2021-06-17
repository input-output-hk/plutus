{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations=4 #-}

-- | This simple escrow contract facilitiates and exchange of currencies.
module Plutus.Contracts.SimpleEscrow
  where

import           Control.Lens             (makeClassyPrisms, review)
import           Control.Monad            (void)
import           Control.Monad.Error.Lens (throwing)
import           Data.Aeson               (FromJSON, ToJSON)
import           GHC.Generics             (Generic)

import           Ledger                   (POSIXTime, PubKeyHash, TxId, txId, txSignedBy, valuePaidTo)
import qualified Ledger
import qualified Ledger.Constraints       as Constraints
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..))
import           Ledger.Interval          (after, before)
import qualified Ledger.Interval          as Interval
import qualified Ledger.Tx                as Tx
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             (Value, geq)

import           Plutus.Contract
import qualified Plutus.Contract.Typed.Tx as Typed
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Applicative (..), Semigroup (..), check, foldMap)

import           Prelude                  (Semigroup (..), foldMap)
import qualified Prelude                  as Haskell

data EscrowParams =
  EscrowParams
    { payee     :: PubKeyHash
    -- ^ The entity that needs to be paid the 'expecting' 'Value'.
    , paying    :: Value
    -- ^ Value to be paid out to the redeemer.
    , expecting :: Value
    -- ^ Value to be received by the payee.
    , deadline  :: POSIXTime
    -- ^ Time after which the contract expires.
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type EscrowSchema =
        Endpoint "lock"   EscrowParams
        .\/ Endpoint "refund" EscrowParams
        .\/ Endpoint "redeem" EscrowParams

data Action
  = Redeem | Refund

data RedeemFailReason = DeadlinePassed
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EscrowError =
    RedeemFailed RedeemFailReason
    | RefundFailed
    | EContractError ContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''EscrowError

instance AsContractError EscrowError where
    _ContractError = _EContractError

newtype RefundSuccess = RefundSuccess TxId
    deriving newtype (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype RedeemSuccess = RedeemSuccess TxId
    deriving (Haskell.Eq, Haskell.Show)

data Escrow
instance Scripts.ValidatorTypes Escrow where
    type instance RedeemerType Escrow = Action
    type instance DatumType    Escrow = EscrowParams

escrowAddress :: Ledger.Address
escrowAddress = Scripts.validatorAddress escrowInstance

escrowInstance :: Scripts.TypedValidator Escrow
escrowInstance = Scripts.mkTypedValidator @Escrow
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @EscrowParams @Action

{-# INLINABLE validate #-}
validate :: EscrowParams -> Action -> ScriptContext -> Bool
validate params action ScriptContext{scriptContextTxInfo=txInfo} =
  case action of
    Redeem ->
          -- Can't redeem after the deadline
      let notLapsed = deadline params `after` txInfoValidRange txInfo
          -- Payee has to have been paid
          paid      = valuePaidTo txInfo (payee params) `geq` expecting params
       in traceIfFalse "escrow-deadline-lapsed" notLapsed
          && traceIfFalse "escrow-not-paid" paid
    Refund ->
          -- Has to be the person that locked value requesting the refund
      let signed = txInfo `txSignedBy` payee params
          -- And we only refund after the deadline has passed
          lapsed = deadline params `before` txInfoValidRange txInfo
       in traceIfFalse "escrow-not-signed" signed
          && traceIfFalse "refund-too-early" lapsed

-- | Lock the 'paying' 'Value' in the output of this script, with the
-- requirement that the transaction validates before the 'deadline'.
lockEp :: Contract () EscrowSchema EscrowError ()
lockEp = do
  params <- endpoint @"lock"
  let valRange = Interval.to (Haskell.pred $ deadline params)
      tx = Constraints.mustPayToTheScript params (paying params)
            <> Constraints.mustValidateIn valRange
  void $ submitTxConstraints escrowInstance tx

-- | Attempts to redeem the 'Value' locked into this script by paying in from
-- the callers address to the payee.
redeemEp :: Contract () EscrowSchema EscrowError RedeemSuccess
redeemEp = mapError (review _EscrowError) $ endpoint @"redeem" >>= redeem
  where
    redeem params = do
      time <- currentTime
      pk <- ownPubKey
      unspentOutputs <- utxoAt escrowAddress

      let value = foldMap (Tx.txOutValue . Tx.txOutTxOut) unspentOutputs
          tx = Typed.collectFromScript unspentOutputs Redeem
                      <> Constraints.mustValidateIn (Interval.to (Haskell.pred $ deadline params))
                      -- Pay me the output of this script
                      <> Constraints.mustPayToPubKey (Ledger.pubKeyHash pk) value
                      -- Pay the payee their due
                      <> Constraints.mustPayToPubKey (payee params) (expecting params)

      if time >= deadline params
      then throwing _RedeemFailed DeadlinePassed
      else RedeemSuccess . txId <$> do submitTxConstraintsSpending escrowInstance unspentOutputs tx

-- | Refunds the locked amount back to the 'payee'.
refundEp :: Contract () EscrowSchema EscrowError RefundSuccess
refundEp = mapError (review _EscrowError) $ endpoint @"refund" >>= refund
  where
    refund params = do
      unspentOutputs <- utxoAt escrowAddress

      let tx = Typed.collectFromScript unspentOutputs Refund
                  <> Constraints.mustValidateIn (Interval.from (Haskell.succ $ deadline params))

      if Constraints.modifiesUtxoSet tx
      then RefundSuccess . txId <$> submitTxConstraintsSpending escrowInstance unspentOutputs tx
      else throwing _RefundFailed ()

PlutusTx.unstableMakeIsData ''EscrowParams
PlutusTx.makeLift ''EscrowParams
PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action
