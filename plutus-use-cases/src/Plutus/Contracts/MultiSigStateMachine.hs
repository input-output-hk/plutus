{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- | A multisig contract written as a state machine.
module Plutus.Contracts.MultiSigStateMachine(
    -- $multisig
      Params(..)
    , Payment(..)
    , State
    , mkValidator
    , typedValidator
    , MultiSigError(..)
    , MultiSigSchema
    , contract
    ) where

import           Control.Lens                 (makeClassyPrisms)
import           Control.Monad                (forever, void)
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Ledger                       (POSIXTime, PubKeyHash, pubKeyHash)
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Constraints           as Constraints
import           Ledger.Contexts              (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts              as Validation
import qualified Ledger.Interval              as Interval
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 (Value)
import qualified Ledger.Value                 as Value

import           Plutus.Contract
import           Plutus.Contract.StateMachine (AsSMContractError, State (..), StateMachine (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..))

import qualified Prelude                      as Haskell

-- $multisig
--   The n-out-of-m multisig contract works like a joint account of
--   m people, requiring the consent of n people for any payments.
--   In the smart contract the signatories are represented by public keys,
--   and approval takes the form of signatures on transactions.
--
--   The multisig contract in
--   'Plutus.Contracts.MultiSig' expects n signatures on
--   a single transaction. This requires an off-chain communication channel. The
--   multisig contract implemented in this module uses a proposal system that
--   allows participants to propose payments and attach their signatures to
--   proposals over a period of time, using separate transactions. All contract
--   state is kept on the chain so there is no need for off-chain communication.

-- | A proposal for making a payment under the multisig scheme.
data Payment = Payment
    { paymentAmount    :: Value
    -- ^ How much to pay out
    , paymentRecipient :: PubKeyHash
    -- ^ Address to pay the value to
    , paymentDeadline  :: POSIXTime
    -- ^ Time until the required amount of signatures has to be collected.
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq Payment where
    {-# INLINABLE (==) #-}
    (Payment vl pk sl) == (Payment vl' pk' sl') = vl == vl' && pk == pk' && sl == sl'


data Params = Params
    { mspSignatories  :: [PubKeyHash]
    -- ^ Public keys that are allowed to authorise payments
    , mspRequiredSigs :: Integer
    -- ^ How many signatures are required for a payment
    }

-- | State of the multisig contract.
data MSState =
    Holding
    -- ^ Money is locked, anyone can make a proposal for a payment. If there is
    -- no value here then this is a final state and the machine will terminate.

    | CollectingSignatures Payment [PubKeyHash]
    -- ^ A payment has been proposed and is awaiting signatures.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq MSState where
    {-# INLINABLE (==) #-}
    Holding == Holding = True
    (CollectingSignatures pmt pks) == (CollectingSignatures pmt' pks') =
        pmt == pmt' && pks == pks'
    _ == _ = False

data Input =
    ProposePayment Payment
    -- ^ Propose a payment. The payment can be made as soon as enough
    --   signatures have been collected.

    | AddSignature PubKeyHash
    -- ^ Add a signature to the sigs. that have been collected for the
    --   current proposal.

    | Cancel
    -- ^ Cancel the current proposal if the deadline has passed

    | Pay
    -- ^ Make the payment.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MultiSigError =
    MSContractError ContractError
    | MSStateMachineError SM.SMContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
makeClassyPrisms ''MultiSigError

instance AsContractError MultiSigError where
    _ContractError = _MSContractError

instance AsSMContractError MultiSigError where
    _SMContractError = _MSStateMachineError

type MultiSigSchema =
        Endpoint "propose-payment" Payment
        .\/ Endpoint "add-signature" ()
        .\/ Endpoint "cancel-payment" ()
        .\/ Endpoint "pay" ()
        .\/ Endpoint "lock" Value

{-# INLINABLE isSignatory #-}
-- | Check if a public key is one of the signatories of the multisig contract.
isSignatory :: PubKeyHash -> Params -> Bool
isSignatory pkh (Params sigs _) = any (\pkh' -> pkh == pkh') sigs

{-# INLINABLE containsPk #-}
-- | Check whether a list of public keys contains a given key.
containsPk :: PubKeyHash -> [PubKeyHash] -> Bool
containsPk pk = any (\pk' -> pk' == pk)

{-# INLINABLE isValidProposal #-}
-- | Check whether a proposed 'Payment' is valid given the total
--   amount of funds currently locked in the contract.
isValidProposal :: Value -> Payment -> Bool
isValidProposal vl (Payment amt _ _) = amt `Value.leq` vl

{-# INLINABLE proposalExpired #-}
-- | Check whether a proposed 'Payment' has expired.
proposalExpired :: TxInfo -> Payment -> Bool
proposalExpired TxInfo{txInfoValidRange} Payment{paymentDeadline} =
    paymentDeadline `Interval.before` txInfoValidRange

{-# INLINABLE proposalAccepted #-}
-- | Check whether enough signatories (represented as a list of public keys)
--   have signed a proposed payment.
proposalAccepted :: Params -> [PubKeyHash] -> Bool
proposalAccepted (Params signatories numReq) pks =
    let numSigned = length (filter (\pk -> containsPk pk pks) signatories)
    in numSigned >= numReq

{-# INLINABLE valuePreserved #-}
-- | @valuePreserved v p@ is true if the pending transaction @p@ pays the amount
--   @v@ to this script's address. It does not assert the number of such outputs:
--   this is handled in the generic state machine validator.
valuePreserved :: Value -> ScriptContext -> Bool
valuePreserved vl ctx = vl == Validation.valueLockedBy (scriptContextTxInfo ctx) (Validation.ownHash ctx)

{-# INLINABLE valuePaid #-}
-- | @valuePaid pm ptx@ is true if the pending transaction @ptx@ pays
--   the amount specified in @pm@ to the public key address specified in @pm@
valuePaid :: Payment -> TxInfo -> Bool
valuePaid (Payment vl pk _) txinfo = vl == Validation.valuePaidTo txinfo pk

{-# INLINABLE transition #-}
transition :: Params -> State MSState -> Input -> Maybe (TxConstraints Void Void, State MSState)
transition params State{ stateData =s, stateValue=currentValue} i = case (s, i) of
    (Holding, ProposePayment pmt)
        | isValidProposal currentValue pmt ->
            Just ( mempty
                 , State
                    { stateData = CollectingSignatures pmt []
                    , stateValue = currentValue
                    }
                 )
    (CollectingSignatures pmt pks, AddSignature pk)
        | isSignatory pk params && not (containsPk pk pks) ->
            let constraints = Constraints.mustBeSignedBy pk in
            Just ( constraints
                 , State
                    { stateData = CollectingSignatures pmt (pk:pks)
                    , stateValue = currentValue
                    }
                 )
    (CollectingSignatures payment _, Cancel) ->
        let constraints = Constraints.mustValidateIn (Interval.from (paymentDeadline payment)) in
        Just ( constraints
             , State
                { stateData = Holding
                , stateValue = currentValue
                }
             )
    (CollectingSignatures payment pkh, Pay)
        | proposalAccepted params pkh ->
            let Payment{paymentAmount, paymentRecipient, paymentDeadline} = payment
                constraints =
                    Constraints.mustValidateIn (Interval.to paymentDeadline)
                    <> Constraints.mustPayToPubKey paymentRecipient paymentAmount
            in Just ( constraints
                    , State
                        { stateData = Holding
                        , stateValue = currentValue - paymentAmount
                        }
                    )
    _ -> Nothing

type MultiSigSym = StateMachine MSState Input

{-# INLINABLE machine #-}
machine :: Params -> MultiSigSym
machine params = SM.mkStateMachine Nothing (transition params) isFinal where
    isFinal _ = False

{-# INLINABLE mkValidator #-}
mkValidator :: Params -> Scripts.ValidatorType MultiSigSym
mkValidator params = SM.mkValidator $ machine params

typedValidator :: Params -> Scripts.TypedValidator MultiSigSym
typedValidator = Scripts.mkTypedValidatorParam @MultiSigSym
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

client :: Params -> SM.StateMachineClient MSState Input
client params = SM.mkStateMachineClient $ SM.StateMachineInstance (machine params) (typedValidator params)

contract ::
    ( AsContractError e
    , AsSMContractError e
    )
    => Params
    -> Contract () MultiSigSchema e ()
contract params = forever endpoints where
    theClient = client params
    endpoints = selectList [lock, propose, cancel, addSignature, pay]
    propose = endpoint @"propose-payment" $ void . SM.runStep theClient . ProposePayment
    cancel  = endpoint @"cancel-payment" $ \() -> void $ SM.runStep theClient Cancel
    addSignature = endpoint @"add-signature" $ \() -> (pubKeyHash <$> ownPubKey) >>= void . SM.runStep theClient . AddSignature
    lock = endpoint @"lock" $ void . SM.runInitialise theClient Holding
    pay = endpoint @"pay" $ \() -> void $ SM.runStep theClient Pay

PlutusTx.unstableMakeIsData ''Payment
PlutusTx.makeLift ''Payment
PlutusTx.unstableMakeIsData ''MSState
PlutusTx.makeLift ''MSState
PlutusTx.makeLift ''Params
PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input
