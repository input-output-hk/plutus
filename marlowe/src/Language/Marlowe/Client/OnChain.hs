{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Client.OnChain(
    mkMarloweClient
    , rolePayoutScript
    , typedValidator
    ) where

import           Data.Default                 (Default (def))
import           Language.Marlowe.Semantics   hiding (Contract)
import           Ledger                       (CurrencySymbol, Datum (..), PubKeyHash, ScriptContext (..), Slot (..),
                                               TokenName, ValidatorHash, mkValidatorScript, validatorHash, valueSpent)
import           Ledger.Ada                   (adaSymbol)
import           Ledger.Constraints
import qualified Ledger.Interval              as Interval
import           Ledger.Scripts               (Validator)
import qualified Ledger.TimeSlot              as TimeSlot
import qualified Ledger.Typed.Scripts         as Scripts
import qualified Ledger.Value                 as Val
import           Plutus.Contract.StateMachine (StateMachine (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
import qualified PlutusTx.Prelude             as P

type MarloweSlotRange = (Slot, Slot)
type MarloweInput = (MarloweSlotRange, [Input])

rolePayoutScript :: CurrencySymbol -> Validator
rolePayoutScript symbol = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode symbol)
  where
    wrapped s = Scripts.wrapValidator (rolePayoutValidator s)

{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
rolePayoutValidator currency role _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role P.> 0

mkRolePayoutValidatorHash :: CurrencySymbol -> ValidatorHash
mkRolePayoutValidatorHash symbol = validatorHash (rolePayoutScript symbol)

defaultRolePayoutValidatorHash :: ValidatorHash
defaultRolePayoutValidatorHash = mkRolePayoutValidatorHash adaSymbol

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rolesCurrency = MarloweParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency }

defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol

{-# INLINABLE mkMarloweStateMachineTransition #-}
mkMarloweStateMachineTransition
    :: MarloweParams
    -> SM.State MarloweData
    -> MarloweInput
    -> Maybe (TxConstraints Void Void, SM.State MarloweData)
mkMarloweStateMachineTransition params SM.State{ SM.stateData=MarloweData{..}, SM.stateValue=scriptInValue}
    (interval@(minSlot, maxSlot), inputs) = do
    let positiveBalances = validateBalances marloweState ||
            P.traceError "Invalid contract state. There exists an account with non positive balance"

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsConstraints = validateInputs params inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = inputBalance == scriptInValue

    let preconditionsOk = P.traceIfFalse "Preconditions are false" $ positiveBalances && balancesOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (outputsConstraints, finalBalance) = case txOutContract of
                    Close -> (payoutConstraints txOutPayments, P.zero)
                    _ -> let
                        outputsConstraints_ = payoutConstraints txOutPayments
                        totalIncome = P.foldMap collectDeposits inputs
                        totalPayouts = P.foldMap (\(Payment _ _ v) -> v) txOutPayments
                        finalBalance_ = totalIncome P.- totalPayouts
                        in (outputsConstraints_, finalBalance_)
            -- TODO Push this use of time further down the code
            let range = TimeSlot.slotRangeToPOSIXTimeRange def $ Interval.interval minSlot maxSlot
            let constraints = inputsConstraints <> outputsConstraints <> mustValidateIn range
            if preconditionsOk
            then Just (constraints, SM.State marloweData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    validateInputs :: MarloweParams -> [Input] -> TxConstraints Void Void
    validateInputs MarloweParams{rolesCurrency} inputs_ = let
        (keys, roles) = P.foldMap validateInputWitness inputs_
        mustSpendSetOfRoleTokens = P.foldMap mustSpendRoleToken (AssocMap.keys roles)
        in P.foldMap mustBeSignedBy keys P.<> mustSpendSetOfRoleTokens
      where
        validateInputWitness :: Input -> ([PubKeyHash], AssocMap.Map TokenName ())
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> (P.mempty, P.mempty)
          where
            validatePartyWitness (PK pk)     = ([pk], P.mempty)
            validatePartyWitness (Role role) = ([], AssocMap.singleton role ())

        mustSpendRoleToken :: TokenName -> TxConstraints Void Void
        mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1

    collectDeposits :: Input -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutConstraints :: [Payment] -> TxConstraints i0 o0
    payoutConstraints payments = P.foldMap payoutToTxOut payoutsByParty
      where
        payoutsByParty = AssocMap.toList $ P.foldMap payoutByParty payments

        payoutToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value

        payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money

        payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

{-# INLINABLE isFinal #-}
isFinal :: MarloweData -> Bool
isFinal MarloweData{marloweContract=c} = c P.== Close

{-# INLINABLE mkValidator #-}
mkValidator :: MarloweParams -> Scripts.ValidatorType MarloweStateMachine
mkValidator p = SM.mkValidator $ SM.mkStateMachine Nothing (mkMarloweStateMachineTransition p) isFinal

mkMarloweValidatorCode
    :: MarloweParams
    -> PlutusTx.CompiledCode (Scripts.ValidatorType MarloweStateMachine)
mkMarloweValidatorCode params =
    $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params

type MarloweStateMachine = StateMachine MarloweData MarloweInput

typedValidator :: MarloweParams -> Scripts.TypedValidator MarloweStateMachine
typedValidator params = Scripts.mkTypedValidator @MarloweStateMachine
    (mkMarloweValidatorCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarloweData @MarloweInput

mkMachineInstance :: MarloweParams -> SM.StateMachineInstance MarloweData MarloweInput
mkMachineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine Nothing (mkMarloweStateMachineTransition params) isFinal)
    (typedValidator params)

mkMarloweClient :: MarloweParams -> SM.StateMachineClient MarloweData MarloweInput
mkMarloweClient params = SM.mkStateMachineClient (mkMachineInstance params)
