{-# LANGUAGE ConstraintKinds       #-}
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
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Client where
import           Control.Lens
import           Control.Monad                (forM_, void)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Data.Aeson                   (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Default                 (Default (def))
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (isNothing, maybeToList)
import           Data.Monoid                  (First (..))
import           Data.Semigroup.Generic       (GenericSemigroupMonoid (..))
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Language.Marlowe.Semantics   hiding (Contract)
import qualified Language.Marlowe.Semantics   as Marlowe
import           Language.Marlowe.Util        (extractContractRoles)
import           Ledger                       (CurrencySymbol, Datum (..), PubKeyHash, ScriptContext (..), Slot (..),
                                               TokenName, TxOut (..), TxOutTx (..), ValidatorHash, eitherTx, inScripts,
                                               mkValidatorScript, pubKeyHash, txOutDatum, txOutValue, txOutputs,
                                               validatorHash, valueSpent)
import qualified Ledger
import           Ledger.Ada                   (adaSymbol, adaValueOf)
import           Ledger.Address               (pubKeyHashAddress, scriptHashAddress)
import           Ledger.AddressMap            (outputsMapFromTxForAddress)
import           Ledger.Constraints
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Interval              as Interval
import           Ledger.Scripts               (Validator, datumHash, unitRedeemer)
import qualified Ledger.TimeSlot              as TimeSlot
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx              (TypedScriptTxOut (..), tyTxOutData)
import qualified Ledger.Value                 as Val
import           Plutus.Contract
import           Plutus.Contract.StateMachine (AsSMContractError (..), StateMachine (..), StateMachineClient (..), Void,
                                               WaitingResult (..), getStates)
import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Currency    as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap            as AssocMap
import qualified PlutusTx.Prelude             as P

type MarloweSlotRange = (Slot, Slot)
type MarloweInput = (MarloweSlotRange, [Input])

type MarloweSchema =
        Endpoint "create" (AssocMap.Map Val.TokenName PubKeyHash, Marlowe.Contract)
        .\/ Endpoint "apply-inputs" (MarloweParams, Maybe SlotInterval, [Input])
        .\/ Endpoint "auto" (MarloweParams, Party, Slot)
        .\/ Endpoint "redeem" (MarloweParams, TokenName, PubKeyHash)
        .\/ Endpoint "close" ()


type MarloweCompanionSchema = EmptySchema
type MarloweFollowSchema = Endpoint "follow" MarloweParams


data MarloweError =
    StateMachineError SM.SMContractError
    | TransitionError (SM.InvalidTransition MarloweData MarloweInput)
    | MarloweEvaluationError TransactionError
    | OtherContractError ContractError
    | RolesCurrencyError Currency.CurrencyError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


makeClassyPrisms ''MarloweError

instance AsSMContractError MarloweError where
    _SMContractError = _StateMachineError

instance AsContractError MarloweError where
    _ContractError = _OtherContractError

instance AsCheckpointError MarloweError where
    _CheckpointError = _OtherContractError . _CheckpointError

instance Currency.AsCurrencyError MarloweError where
    _CurrencyError = _RolesCurrencyError

data PartyAction
             = PayDeposit AccountId Party Token Integer
             | WaitForTimeout Slot
             | WaitOtherActionUntil Slot
             | NotSure
             | CloseContract
  deriving (Show)

type RoleOwners = AssocMap.Map Val.TokenName PubKeyHash

data ContractHistory =
    ContractHistory
        { chParams  :: First (MarloweParams, MarloweData)
        , chHistory :: [TransactionInput]
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ContractHistory)

created :: MarloweParams -> MarloweData -> ContractHistory
created p d = mempty{chParams = First (Just (p, d)) }

transition :: TransactionInput -> ContractHistory
transition i = mempty{chHistory = [i] }

isEmpty :: ContractHistory -> Bool
isEmpty = isNothing . getFirst . chParams

data ContractProgress = InProgress | Finished
  deriving (Show,Eq)

instance Semigroup ContractProgress where
    _ <> Finished     = Finished
    any <> InProgress = any

instance Monoid ContractProgress where
    mempty = InProgress


data LastResult = OK | SomeError MarloweError | Unknown
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup LastResult where
    any <> Unknown = any
    _ <> last      = last

instance Monoid LastResult where
    mempty = Unknown

type MarloweContractState = LastResult


marloweFollowContract :: Contract ContractHistory MarloweFollowSchema MarloweError (Waited ())
marloweFollowContract = endpoint @"follow" $ \params -> do
    slot <- currentSlot
    checkpointLoop follow (0, slot, params)
  where
    follow (ifrom, ito, params) = do
        let client@StateMachineClient{scInstance} = mkMarloweClient params
        let inst = SM.typedValidator scInstance
        let address = Scripts.validatorAddress inst
        AddressChangeResponse{acrTxns} <- getWaited <$> addressChangeRequest
                AddressChangeRequest
                { acreqSlotRangeFrom = ifrom
                , acreqSlotRangeTo = ito
                , acreqAddress = address
                }
        let go [] = pure InProgress
            go (tx:rest) = do
                res <- updateHistoryFromTx client params tx
                case res of
                    Finished   -> pure Finished
                    InProgress -> go rest
        res <- go acrTxns
        case res of
            Finished -> do
                logDebug @String ("Contract finished " <> show params)
                pure $ Left () -- close the contract
            InProgress ->
                let next = succ ito in
                pure $ Right (next, next, params)

    updateHistoryFromTx StateMachineClient{scInstance, scChooser} params tx = do
        logInfo @String $ "Updating history from tx" <> show (Ledger.eitherTx Ledger.txId Ledger.txId tx)
        let inst = SM.typedValidator scInstance
        let address = Scripts.validatorAddress inst
        let utxo = outputsMapFromTxForAddress address tx
        let states = getStates scInstance utxo
        case findInput inst tx of
            -- if there's no TxIn for Marlowe contract that means
            -- it's a contract creation transaction, and there is Marlowe TxOut
            Nothing -> case scChooser states of
                Left err    -> throwing _SMContractError err
                Right (state, _) -> do
                    let initialMarloweData = tyTxOutData state
                    logInfo @String ("Contract created " <> show initialMarloweData)
                    tell $ created params initialMarloweData
                    pure InProgress
            -- There is TxIn with Marlowe contract, hence this is a state transition
            Just (interval, inputs) -> do
                let txInput = TransactionInput {
                        txInterval = interval,
                        txInputs = inputs }
                tell $ transition txInput
                case states of
                    -- when there is no Marlowe TxOut the contract is closed
                    -- and we can close the follower contract
                    [] -> pure Finished
                    -- otherwise we continue following
                    _  -> pure InProgress

    findInput inst tx = do
        let txIns = Set.toList (Ledger.consumableInputs tx)
        let inputs = txIns >>= (maybeToList . inScripts)
        let script = Scripts.validatorScript inst
        -- find previous Marlowe contract
        let marloweTxInputs = filter (\(validator, _, _) -> validator == script) inputs
        case marloweTxInputs of
            []                          -> Nothing
            [(_, Ledger.Redeemer d, _)] -> PlutusTx.fromBuiltinData d
            _                           -> Nothing

{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = do
    catching _MarloweError
        (void $ mapError (review _MarloweError) $
            selectList [create, apply, auto, redeem, close])
        (\er -> do
            tell $ SomeError er
            marlowePlutusContract)
  where
    create = endpoint @"create" $ \(owners, contract) -> do
        (params, distributeRoleTokens) <- setupMarloweParams owners contract
        slot <- currentSlot
        let StateMachineClient{scInstance} = mkMarloweClient params
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = emptyState slot }
        let payValue = adaValueOf 0
        let SM.StateMachineInstance{SM.typedValidator} = scInstance
        let tx = mustPayToTheScript marloweData payValue <> distributeRoleTokens
        let lookups = Constraints.typedValidatorLookups typedValidator
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
        submitTxConfirmed utx
        marlowePlutusContract
    apply = endpoint @"apply-inputs" $ \(params, slotInterval, inputs) -> do
        _ <- applyInputs params slotInterval inputs
        tell OK
        marlowePlutusContract
    redeem = mapError (review _MarloweError) $ endpoint @"redeem" $ \(MarloweParams{rolesCurrency}, role, pkh) -> do
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        utxos <- utxoAt address
        let spendPayoutConstraints tx ref TxOutTx{txOutTxOut} = let
                expectedDatumHash = datumHash (Datum $ PlutusTx.toBuiltinData role)
                amount = txOutValue txOutTxOut
                in case txOutDatum txOutTxOut of
                    Just datumHash | datumHash == expectedDatumHash ->
                        -- we spend the rolePayoutScript address
                        Constraints.mustSpendScriptOutput ref unitRedeemer
                        -- and pay to a token owner
                            <> Constraints.mustPayToPubKey pkh amount
                    _ -> tx

        let spendPayouts = Map.foldlWithKey spendPayoutConstraints mempty utxos
            constraints = spendPayouts
                -- must spend a role token for authorization
                <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
            -- lookup for payout validator and role payouts
            validator = rolePayoutScript rolesCurrency
            lookups = Constraints.otherScript validator
                <> Constraints.unspentOutputs utxos
                <> Constraints.ownPubKeyHash pkh
        tx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx @Void lookups constraints)
        _ <- submitUnbalancedTx tx
        tell OK
        marlowePlutusContract
    auto = endpoint @"auto" $ \(params, party, untilSlot) -> do
        let theClient = mkMarloweClient params
        let continueWith :: MarloweData -> Contract MarloweContractState MarloweSchema MarloweError ()
            continueWith md@MarloweData{marloweContract} =
                if canAutoExecuteContractForParty party marloweContract
                then autoExecuteContract theClient party md
                else do
                    tell OK
                    marlowePlutusContract

        maybeState <- SM.getOnChainState theClient
        case maybeState of
            Nothing -> do
                wr <- getWaited <$> SM.waitForUpdateUntilSlot theClient untilSlot
                case wr of
                    ContractEnded -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        tell OK
                        marlowePlutusContract
                    Timeout _ -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        tell OK
                        marlowePlutusContract
                    WaitingResult marloweData -> continueWith marloweData
            Just ((st, _), _) -> do
                let marloweData = tyTxOutData st
                continueWith marloweData
    close = endpoint @"close" $ \_ -> tell OK


    autoExecuteContract :: StateMachineClient MarloweData MarloweInput
                      -> Party
                      -> MarloweData
                      -> Contract MarloweContractState MarloweSchema MarloweError ()
    autoExecuteContract theClient party marloweData = do
        slot <- currentSlot
        let slotRange = (slot, slot + defaultTxValidationRange)
        let action = getAction slotRange party marloweData
        case action of
            PayDeposit acc p token amount -> do
                logInfo @String $ "PayDeposit " <> show amount <> " at whithin slots " <> show slotRange
                let payDeposit = do
                        marloweData <- SM.runStep theClient (slotRange, [IDeposit acc p token amount])
                        case marloweData of
                            SM.TransitionFailure e -> throwing _TransitionError e
                            SM.TransitionSuccess d -> continueWith d

                catching (_StateMachineError) payDeposit $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 slots"
                    _ <- awaitSlot (slot + 2)
                    continueWith marloweData
            WaitForTimeout timeout -> do
                logInfo @String $ "WaitForTimeout " <> show timeout
                _ <- awaitSlot timeout
                continueWith marloweData
            WaitOtherActionUntil timeout -> do
                logInfo @String $ "WaitOtherActionUntil " <> show timeout
                wr <- getWaited <$> SM.waitForUpdateUntilSlot theClient timeout
                case wr of
                    ContractEnded -> do
                        logInfo @String $ "Contract Ended"
                        tell OK
                        marlowePlutusContract
                    Timeout _ -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith marloweData
                    WaitingResult marloweData -> continueWith marloweData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                tell OK
                marlowePlutusContract

            NotSure -> do
                logInfo @String $ "NotSure"
                tell OK
                marlowePlutusContract

          where
            continueWith = autoExecuteContract theClient party


setupMarloweParams
    :: forall s e i o.
    (AsMarloweError e)
    => RoleOwners -> Marlowe.Contract -> Contract MarloweContractState s e (MarloweParams, TxConstraints i o)
setupMarloweParams owners contract = mapError (review _MarloweError) $ do
    creator <- pubKeyHash <$> ownPubKey
    let roles = extractContractRoles contract
    if Set.null roles
    then do
        let params = MarloweParams
                { rolesCurrency = adaSymbol
                , rolePayoutValidatorHash = defaultRolePayoutValidatorHash }
        pure (params, mempty)
    else if roles `Set.isSubsetOf` Set.fromList (AssocMap.keys owners)
    then do
        let tokens = fmap (\role -> (role, 1)) $ Set.toList roles
        cur <- mapError RolesCurrencyError $ Currency.mintContract creator tokens
        let rolesSymbol = Currency.currencySymbol cur
        let giveToParty (role, pkh) = Constraints.mustPayToPubKey pkh (Val.singleton rolesSymbol role 1)
        let distributeRoleTokens = foldMap giveToParty (AssocMap.toList owners)
        let params = MarloweParams
                { rolesCurrency = rolesSymbol
                , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesSymbol }
        pure (params, distributeRoleTokens)
    else do
        let missingRoles = roles `Set.difference` Set.fromList (AssocMap.keys owners)
        let message = T.pack $ "You didn't specify owners of these roles: " <> show missingRoles
        throwing _ContractError $ OtherError message


getAction :: MarloweSlotRange -> Party -> MarloweData -> PartyAction
getAction slotRange party MarloweData{marloweContract,marloweState} = let
    env = Environment slotRange
    in case reduceContractUntilQuiescent env marloweState marloweContract of
        ContractQuiescent _warnings _payments state contract ->
            -- here the contract is either When or Close
            case contract of
                When [Case (Deposit acc depositParty tok value) _] _ _
                    | party == depositParty -> let
                        amount = Marlowe.evalValue env state value
                        in PayDeposit acc party tok amount
                When [Case (Deposit _ depositParty _ _) _] timeout _
                    | party /= depositParty    ->
                        WaitOtherActionUntil timeout
                When [] timeout _ -> WaitForTimeout timeout
                Close -> CloseContract
                _ -> NotSure
        -- When's timeout is in the slot range
        RRAmbiguousSlotIntervalError ->
            {- FIXME
                Consider contract:
                    When [cases] (Slot 100) (When [Case Deposit Close]] (Slot 105) Close)

                For a slot range (95, 105) we get RRAmbiguousSlotIntervalError
                because timeout 100 is inside the slot range.
                Now, we wait for slot 105, and we miss the Deposit.

                To avoid that we need to know what was the original timeout
                that caused RRAmbiguousSlotIntervalError (i.e. Slot 100).
                Then we'd rather wait until slot 100 instead and would make the Deposit.
                I propose to modify RRAmbiguousSlotIntervalError to include the expected timeout.
             -}
            WaitForTimeout (snd slotRange)



canAutoExecuteContractForParty :: Party -> Marlowe.Contract -> Bool
canAutoExecuteContractForParty party = check
  where
    check cont =
        case cont of
            Close                                      -> True
            When [] _ cont                             -> check cont
            When [Case (Deposit{}) cont] _ timeoutCont -> check cont && check timeoutCont
            When cases _ timeoutCont                   -> all checkCase cases && check timeoutCont
            Pay _ _ _ _ cont                           -> check cont
            If _ c1 c2                                 -> check c1 && check c2
            Let _ _ cont                               -> check cont
            Assert _ cont                              -> check cont


    checkCase (Case (Choice (ChoiceId _ p) _) cont) | p /= party = check cont
    checkCase _                                     = False


applyInputs :: AsMarloweError e
    => MarloweParams
    -> Maybe SlotInterval
    -> [Input]
    -> Contract MarloweContractState MarloweSchema e MarloweData
applyInputs params slotInterval inputs = mapError (review _MarloweError) $ do
    slotRange <- case slotInterval of
            Just si -> pure si
            Nothing -> do
                slot <- currentSlot
                pure (slot, slot + defaultTxValidationRange)
    let theClient = mkMarloweClient params
    dat <- SM.runStep theClient (slotRange, inputs)
    case dat of
        SM.TransitionFailure e -> do
            logError e
            throwing _TransitionError e
        SM.TransitionSuccess d -> return d

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
                        outputsConstraints = payoutConstraints txOutPayments
                        totalIncome = P.foldMap collectDeposits inputs
                        totalPayouts = P.foldMap (\(Payment _ _ v) -> v) txOutPayments
                        finalBalance = totalIncome P.- totalPayouts
                        in (outputsConstraints, finalBalance)
            -- TODO Push this use of time further down the code
            let range = TimeSlot.slotRangeToPOSIXTimeRange def $ Interval.interval minSlot maxSlot
            let constraints = inputsConstraints <> outputsConstraints <> mustValidateIn range
            if preconditionsOk
            then Just (constraints, SM.State marloweData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    validateInputs :: MarloweParams -> [Input] -> TxConstraints Void Void
    validateInputs MarloweParams{rolesCurrency} inputs = let
        (keys, roles) = P.foldMap validateInputWitness inputs
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


defaultTxValidationRange :: Slot
defaultTxValidationRange = 10


newtype CompanionState = CompanionState (Map MarloweParams MarloweData)
  deriving (Semigroup,Monoid) via (Map MarloweParams MarloweData)

instance ToJSON CompanionState where
    toJSON (CompanionState m) = toJSON $ Map.toList m

instance FromJSON CompanionState where
    parseJSON v = CompanionState . Map.fromList <$> parseJSON v

{-|
    Contract that monitors a user wallet for receiving a Marlowe role token.
    When it sees that a Marlowe contract exists on chain with a role currency
    of a token the user owns it updates its @CompanionState@
    with contract's @MarloweParams@ and @MarloweData@
-}
marloweCompanionContract :: Contract CompanionState MarloweCompanionSchema MarloweError ()
marloweCompanionContract = contracts
  where
    contracts = do
        pkh <- pubKeyHash <$> ownPubKey
        let ownAddress = pubKeyHashAddress pkh
        utxo <- utxoAt ownAddress
        let txOuts = fmap (txOutTxOut . snd) $ Map.toList utxo
        forM_ txOuts notifyOnNewContractRoles
        checkpointLoop (fmap Right <$> cont) ownAddress
    cont ownAddress = do
        txns <- getWaited <$> nextTransactionsAt ownAddress
        let txOuts = txns >>= eitherTx (const []) txOutputs
        forM_ txOuts notifyOnNewContractRoles
        pure ownAddress

notifyOnNewContractRoles :: TxOut
    -> Contract CompanionState MarloweCompanionSchema MarloweError ()
notifyOnNewContractRoles txout = do
    let curSymbols = filterRoles txout
    forM_ curSymbols $ \cs -> do
        contract <- findMarloweContractsOnChainByRoleCurrency cs
        case contract of
            Just (params, md) -> do
                logDebug @String $ "Updating observable state"
                tell $ CompanionState (Map.singleton params md)
            Nothing           -> do
                logWarn @String $ "On-chain state not found!"
                pure ()


filterRoles :: TxOut -> [CurrencySymbol]
filterRoles TxOut { txOutValue, txOutDatumHash = Nothing } =
    let curSymbols = filter (/= adaSymbol) $ AssocMap.keys $ Val.getValue txOutValue
    in  curSymbols
filterRoles _ = []


findMarloweContractsOnChainByRoleCurrency
    :: CurrencySymbol
    -> Contract CompanionState
                MarloweCompanionSchema
                MarloweError
                (Maybe (MarloweParams, MarloweData))
findMarloweContractsOnChainByRoleCurrency curSym = do
    let params = marloweParams curSym
    let client = mkMarloweClient params
    maybeState <- SM.getOnChainState client
    case maybeState of
        Just ((st, _), _) -> do
            let marloweData = tyTxOutData st
            pure $ Just (params, marloweData)
        Nothing -> pure Nothing
