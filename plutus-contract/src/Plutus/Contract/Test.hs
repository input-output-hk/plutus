{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Testing contracts with HUnit and Tasty
module Plutus.Contract.Test(
      module X
    , TracePredicate
    , ContractConstraints
    , Plutus.Contract.Test.not
    , (.&&.)
    -- * Assertions
    , endpointAvailable
    , queryingUtxoAt
    , assertDone
    , assertNotDone
    , assertContractError
    , Outcome(..)
    , assertOutcome
    , assertInstanceLog
    , assertNoFailedTransactions
    , assertFailedTransaction
    , assertHooks
    , assertResponses
    , assertUserLog
    , assertBlockchain
    , assertChainEvents
    , assertAccumState
    , Shrinking(..)
    , assertResumableResult
    , tx
    , anyTx
    , assertEvents
    , walletFundsChange
    , walletFundsExactChange
    , walletPaidFees
    , waitingForSlot
    , walletWatchingAddress
    , valueAtAddress
    , dataAtAddress
    , reasonable
    , reasonable'
    -- * Checking predicates
    , checkPredicate
    , checkPredicateOptions
    , checkPredicateGen
    , checkPredicateGenOptions
    , checkPredicateInner
    , CheckOptions
    , defaultCheckOptions
    , minLogLevel
    , maxSlot
    , emulatorConfig
    -- * Etc
    , goldenPir
    ) where

import           Control.Applicative                   (liftA2)
import           Control.Foldl                         (FoldM)
import qualified Control.Foldl                         as L
import           Control.Lens                          (at, makeLenses, preview, to, (&), (.~), (^.))
import           Control.Monad                         (unless)
import           Control.Monad.Freer                   (Eff, reinterpret, runM, sendM)
import           Control.Monad.Freer.Error             (Error, runError)
import           Control.Monad.Freer.Extras.Log        (LogLevel (..), LogMessage (..))
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.Writer            (Writer (..), tell)
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Data.Default                          (Default (..))
import           Data.Foldable                         (fold, toList, traverse_)
import           Data.Maybe                            (fromJust, mapMaybe)
import           Data.Proxy                            (Proxy (..))
import           Data.String                           (IsString (..))
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void
import           GHC.TypeLits                          (KnownSymbol, Symbol, symbolVal)
import           Plutus.Contract.Effects               (ActiveEndpoint (..), PABReq, PABResp)


import           Hedgehog                              (Property, forAll, property)
import qualified Hedgehog
import           Test.Tasty.Golden                     (goldenVsString)
import qualified Test.Tasty.HUnit                      as HUnit
import           Test.Tasty.Providers                  (TestTree)

import qualified Ledger.Ada                            as Ada
import           Ledger.Constraints.OffChain           (UnbalancedTx)
import           Ledger.Tx                             (Tx)
import qualified Plutus.Contract.Effects               as Requests
import qualified Plutus.Contract.Request               as Request
import           Plutus.Contract.Resumable             (Request (..), Response (..))
import qualified Plutus.Contract.Resumable             as State
import           Plutus.Contract.Types                 (Contract (..), ResumableResult, shrinkResumableResult)
import           PlutusTx                              (CompiledCode, IsData (..), getPir)
import qualified PlutusTx.Prelude                      as P

import           Ledger                                (Validator)
import qualified Ledger
import           Ledger.Address                        (Address)
import           Ledger.Generators                     (GeneratorModel, Mockchain (..))
import qualified Ledger.Generators                     as Gen
import           Ledger.Index                          (ScriptValidationEvent, ValidationError)
import           Ledger.Slot                           (Slot)
import           Ledger.Value                          (Value)
import           Wallet.Emulator                       (EmulatorEvent, EmulatorTimeEvent)

import           Plutus.Contract.Trace                 as X
import           Plutus.Trace.Emulator                 (EmulatorConfig (..), EmulatorTrace, runEmulatorStream)
import           Plutus.Trace.Emulator.Types           (ContractConstraints, ContractInstanceLog,
                                                        ContractInstanceState (..), ContractInstanceTag, UserThreadMsg)
import qualified Streaming                             as S
import qualified Streaming.Prelude                     as S
import           Wallet.Emulator.Chain                 (ChainEvent)
import           Wallet.Emulator.Folds                 (EmulatorFoldErr (..), Outcome (..), describeError, postMapM)
import qualified Wallet.Emulator.Folds                 as Folds
import           Wallet.Emulator.Stream                (filterLogLevel, foldEmulatorStreamM, initialChainState,
                                                        initialDist, takeUntilSlot)

type TracePredicate = FoldM (Eff '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void)]) EmulatorEvent Bool

infixl 3 .&&.

(.&&.) :: TracePredicate -> TracePredicate -> TracePredicate
(.&&.) = liftA2 (&&)

not :: TracePredicate -> TracePredicate
not = fmap Prelude.not

-- | Options for running the
data CheckOptions =
    CheckOptions
        { _minLogLevel    :: LogLevel -- ^ Minimum log level for emulator log messages to be included in the test output (printed if the test fails)
        , _maxSlot        :: Slot -- ^ When to stop the emulator
        , _emulatorConfig :: EmulatorConfig
        } deriving (Eq, Show)

makeLenses ''CheckOptions

defaultCheckOptions :: CheckOptions
defaultCheckOptions =
    CheckOptions
        { _minLogLevel = Info
        , _maxSlot = 125
        , _emulatorConfig = def
        }

type TestEffects = '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void)]

-- | Check if the emulator trace meets the condition
checkPredicate ::
    String -- ^ Descriptive name of the test
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicate = checkPredicateOptions defaultCheckOptions

-- | Check if the emulator trace meets the condition, using the
--   'GeneratorModel' to generate initial transactions for the blockchain
checkPredicateGen ::
    GeneratorModel
    -> TracePredicate
    -> EmulatorTrace ()
    -> Property
checkPredicateGen = checkPredicateGenOptions defaultCheckOptions

-- | Evaluate a trace predicate on an emulator trace, printing out debug information
--   and making assertions as we go.
checkPredicateInner :: forall m.
    Monad m
    => CheckOptions
    -> TracePredicate
    -> EmulatorTrace ()
    -> (String -> m ()) -- ^ Print out debug information in case of test failures
    -> (Bool -> m ()) -- ^ assert
    -> m ()
checkPredicateInner CheckOptions{_minLogLevel, _maxSlot, _emulatorConfig} predicate action annot assert = do
    let dist = _emulatorConfig ^. initialChainState . to initialDist
        theStream :: forall effs. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ()
        theStream = takeUntilSlot _maxSlot $ runEmulatorStream _emulatorConfig action
        consumeStream :: forall a. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff TestEffects) a -> Eff TestEffects (S.Of Bool a)
        consumeStream = foldEmulatorStreamM @TestEffects predicate
    result <- runM
                $ reinterpret @(Writer (Doc Void)) @m  (\case { Tell d -> sendM $ annot $ Text.unpack $ renderStrict $ layoutPretty defaultLayoutOptions d })
                $ runError
                $ runReader dist
                $ consumeStream theStream

    unless (fmap S.fst' result == Right True) $ do
        annot "Test failed."
        annot "Emulator log:"
        S.mapM_ annot
            $ S.hoist runM
            $ S.map (Text.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty)
            $ filterLogLevel _minLogLevel
            theStream

        case result of
            Left err -> do
                annot "Error:"
                annot (describeError err)
                annot (show err)
                assert False
            Right _ -> assert False

-- | A version of 'checkPredicateGen' with configurable 'CheckOptions'
checkPredicateGenOptions ::
    CheckOptions
    -> GeneratorModel
    -> TracePredicate
    -> EmulatorTrace ()
    -> Property
checkPredicateGenOptions options gm predicate action = property $ do
    Mockchain{mockchainInitialTxPool} <- forAll (Gen.genMockchain' gm)
    let options' = options & emulatorConfig . initialChainState .~ Right mockchainInitialTxPool
    checkPredicateInner options' predicate action Hedgehog.annotate Hedgehog.assert

-- | A version of 'checkPredicate' with configurable 'CheckOptions'
checkPredicateOptions ::
    CheckOptions -- ^ Options to use
    -> String -- ^ Descriptive name of the test
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicateOptions options nm predicate action = do
    HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner options predicate action step (HUnit.assertBool nm)

endpointAvailable
    :: forall (l :: Symbol) w s e a.
       ( KnownSymbol l
       , Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> TracePredicate
endpointAvailable contract inst =
    let desc = Request.endpointDescription (Proxy @l) in
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks :: [Request ActiveEndpoint]
            hks = mapMaybe (traverse (preview Requests._ExposeEndpointReq)) rqs
        if any (\ActiveEndpoint{aeDescription} -> aeDescription == desc) (rqRequest <$> hks)
            then pure True
            else do
                tell @(Doc Void) ("missing endpoint:" <+> fromString (symbolVal (Proxy :: Proxy l)))
                pure False

queryingUtxoAt
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> Address
    -> TracePredicate
queryingUtxoAt contract inst addr =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks :: [Request Address]
            hks = mapMaybe (traverse (preview Requests._UtxoAtReq)) rqs
        if elem addr (rqRequest <$> hks)
        then pure True
        else do
            tell @(Doc Void) $ hsep
                [ "UTXO queries of " <+> pretty inst <> colon
                    <+> nest 2 (concatWith (surround (comma <> space))  (viaShow <$> toList hks))
                , "Missing address:", viaShow addr
                ]
            pure False

tx
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> (UnbalancedTx -> Bool)
    -> String
    -> TracePredicate
tx contract inst flt nm =
    flip postMapM (Folds.instanceTransactions contract inst) $ \unbalancedTxns -> do
        if any flt unbalancedTxns
        then pure True
        else do
            tell @(Doc Void) $ hsep
                [ "Unbalanced transactions of" <+> pretty inst <> colon
                    <+> nest 2 (vsep (fmap pretty unbalancedTxns))
                , "No transaction with '" <> fromString nm <> "'"]
            pure False

walletWatchingAddress :: Wallet -> Address -> TracePredicate
walletWatchingAddress w addr = flip postMapM (L.generalize $ Folds.walletWatchingAddress w addr) $ \r -> do
    unless r $ do
        tell @(Doc Void) $ "Wallet" <+> pretty w <+> "not watching address" <+> pretty addr
    pure r

assertEvents
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> ([PABResp] -> Bool)
    -> String
    -> TracePredicate
assertEvents contract inst pr nm =
    flip postMapM (Folds.instanceResponses contract inst) $ \rqs -> do
        let responses = fmap State.rspResponse rqs
            result = pr responses
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Event log for" <+> pretty inst <> ":"
                , nest 2 (vsep (fmap pretty responses))
                , "Fails" <+> squotes (fromString nm)
                ]
        pure result

-- | Check that the funds at an address meet some condition.
valueAtAddress :: Address -> (Value -> Bool) -> TracePredicate
valueAtAddress address check =
    flip postMapM (L.generalize $ Folds.valueAtAddress address) $ \vl -> do
        let result = check vl
        unless result $ do
            tell @(Doc Void) ("Funds at address" <+> pretty address <+> "were" <+> pretty vl)
        pure result

dataAtAddress :: IsData a => Address -> (a -> Bool) -> TracePredicate
dataAtAddress address check =
    flip postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
        let isSingletonWith p xs = length xs == 1 && all p xs
        let result = isSingletonWith (isSingletonWith (maybe False check . fromBuiltinData . Ledger.getDatum) . Ledger.txData . Ledger.txOutTxTx) utxo
        unless result $ do
            tell @(Doc Void) ("Data at address" <+> pretty address <+> "was"
                <+> foldMap (foldMap pretty . Ledger.txData . Ledger.txOutTxTx) utxo)
        pure result

waitingForSlot
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> Slot
    -> TracePredicate
waitingForSlot contract inst sl =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks :: [Request Slot]
            hks = mapMaybe (traverse (preview Requests._AwaitSlotReq)) rqs
        case filter ((==) sl) (rqRequest <$> hks) of
            [] -> do
                tell @(Doc Void) $ pretty inst <+> "not waiting for any slot notifications. Expected:" <+>  viaShow sl
                pure False
            _ -> pure True

anyTx
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> TracePredicate
anyTx contract inst = tx contract inst (const True) "anyTx"

assertHooks
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> ([PABReq] -> Bool)
    -> String
    -> TracePredicate
assertHooks contract inst p nm =
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks = rqRequest <$> rqs
            result = p hks
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Handlers for" <+> pretty inst <> colon
                , nest 2 (pretty hks)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Make an assertion about the responses provided to the contract instance.
assertResponses
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> ([Response PABResp] -> Bool)
    -> String
    -> TracePredicate
assertResponses contract inst p nm =
    flip postMapM (Folds.instanceResponses contract inst) $ \rqs -> do
        let result = p rqs
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Record:"
                , nest 2 (pretty rqs)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

data Shrinking = DoShrink | DontShrink
    deriving (Eq, Ord, Show)

-- | make an assertion about the 'ContractInstanceState' of a contract
--   instance
assertResumableResult ::
    forall w s e a.
    ( Monoid w
    , Show e
    , Show a
    , Show w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> Shrinking
    -> (ResumableResult w e PABResp PABReq a -> Bool)
    -> String
    -> TracePredicate
assertResumableResult contract inst shrinking p nm =
    let f = case shrinking of { DontShrink -> id; DoShrink -> shrinkResumableResult } in
    flip postMapM (Folds.instanceState contract inst) $ \case
        Nothing -> do
            tell @(Doc Void) $ "No state for " <+> pretty inst
            pure False
        Just ContractInstanceState{instContractState} -> do
            let shrunkState = f instContractState
                result = p shrunkState
            unless result $ do
                tell @(Doc Void) $ vsep
                    [ "Resumable result for" <+> pretty inst
                    , viaShow shrunkState
                    , "Failed" <+> squotes (fromString nm)
                    ]
            pure result

-- | A 'TracePredicate' checking that the wallet's contract instance finished
--   without errors.
assertDone
    :: forall w s e a.
    ( Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> (a -> Bool)
    -> String
    -> TracePredicate
assertDone contract inst pr = assertOutcome contract inst (\case { Done a -> pr a; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance is
--   waiting for input.
assertNotDone
    :: forall w s e a.
    ( Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> String
    -> TracePredicate
assertNotDone contract inst = assertOutcome contract inst (\case { NotDone -> True; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance
--   failed with an error.
assertContractError
    :: forall w s e a.
    ( Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> (e -> Bool)
    -> String
    -> TracePredicate
assertContractError contract inst p = assertOutcome contract inst (\case { Failed err -> p err; _ -> False })

assertOutcome
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> (Outcome e a -> Bool)
    -> String
    -> TracePredicate
assertOutcome contract inst p nm =
    flip postMapM (Folds.instanceOutcome contract inst) $ \outcome -> do
        let result = p outcome
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Outcome of" <+> pretty inst <> colon
                , indent 2 (viaShow result)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Check that the funds in the wallet have changed by the given amount, exluding fees.
walletFundsChange :: Wallet -> Value -> TracePredicate
walletFundsChange = walletFundsChangeImpl False

-- | Check that the funds in the wallet have changed by the given amount, including fees.
walletFundsExactChange :: Wallet -> Value -> TracePredicate
walletFundsExactChange = walletFundsChangeImpl True

walletFundsChangeImpl :: Bool -> Wallet -> Value -> TracePredicate
walletFundsChangeImpl exact w dlt =
    flip postMapM (L.generalize $ (,) <$> Folds.walletFunds w <*> Folds.walletFees w) $ \(finalValue', fees) -> do
        dist <- ask @InitialDistribution
        let initialValue = fold (dist ^. at w)
            finalValue = finalValue' P.+ if exact then mempty else fees
            result = initialValue P.+ dlt == finalValue
        unless result $ do
            tell @(Doc Void) $ vsep $
                [ "Expected funds of" <+> pretty w <+> "to change by"
                , " " <+> viaShow dlt] ++
                (if exact then [] else ["  (excluding" <+> viaShow (Ada.getLovelace (Ada.fromValue fees)) <+> "lovelace in fees)" ]) ++
                if initialValue == finalValue
                then ["but they did not change"]
                else ["but they changed by", " " <+> viaShow (finalValue P.- initialValue)]
        pure result

walletPaidFees :: Wallet -> Value -> TracePredicate
walletPaidFees w val =
    flip postMapM (L.generalize $ Folds.walletFees w) $ \fees -> do
        let result = fees == val
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Expected" <+> pretty w <+> "to pay"
                , " " <+> viaShow val
                , "lovelace in fees, but they paid"
                , " " <+> viaShow fees ]
        pure result

-- | An assertion about the blockchain
assertBlockchain :: ([Ledger.Block] -> Bool) -> TracePredicate
assertBlockchain predicate =
    flip postMapM (L.generalize Folds.blockchain) $ \chain -> do
        let passing = predicate chain
        unless passing $ do
            tell @(Doc Void) $ "Blockchain does not match predicate."
        pure passing

-- | An assertion about the chain events
assertChainEvents :: ([ChainEvent] -> Bool) -> TracePredicate
assertChainEvents predicate =
    flip postMapM (L.generalize Folds.chainEvents) $ \evts -> do
        let passing = predicate evts
        unless passing $ do
            tell @(Doc Void) $ "Chain events do not match predicate."
            traverse_ (tell @(Doc Void) . pretty) evts
        pure passing

-- | Assert that at least one transaction failed to validate, and that all
--   transactions that failed meet the predicate.
assertFailedTransaction :: (Tx -> ValidationError -> [ScriptValidationEvent] -> Bool) -> TracePredicate
assertFailedTransaction predicate =
    flip postMapM (L.generalize $ Folds.failedTransactions Nothing) $ \case
        [] -> do
            tell @(Doc Void) $ "No transactions failed to validate."
            pure False
        xs -> pure (all (\(_, t, e, evts) -> predicate t e evts) xs)

-- | Assert that no transaction failed to validate.
assertNoFailedTransactions :: TracePredicate
assertNoFailedTransactions =
    flip postMapM (L.generalize $ Folds.failedTransactions Nothing) $ \case
        [] -> pure True
        xs -> do
            let prettyTxFail (i, _, err, _) = pretty i <> colon <+> pretty err
            tell @(Doc Void) $ vsep ("Transactions failed to validate:" : fmap prettyTxFail xs)
            pure False

assertInstanceLog ::
    ContractInstanceTag
    -> ([EmulatorTimeEvent ContractInstanceLog] -> Bool)
    -> TracePredicate
assertInstanceLog tag pred' = flip postMapM (L.generalize $ Folds.instanceLog tag) $ \lg -> do
    let result = pred' lg
    unless result (tell @(Doc Void) $ vsep ("Contract instance log failed to validate:" : fmap pretty lg))
    pure result

assertUserLog ::
    ([EmulatorTimeEvent UserThreadMsg] -> Bool)
    -> TracePredicate
assertUserLog pred' = flip postMapM (L.generalize Folds.userLog) $ \lg -> do
    let result = pred' lg
    unless result (tell @(Doc Void) $ vsep ("User log failed to validate:" : fmap pretty lg))
    pure result

-- | Make an assertion about the accumulated state @w@ of
--   a contract instance.
assertAccumState ::
    forall w s e a.
    ( Monoid w
    , Show w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> (w -> Bool)
    -> String
    -> TracePredicate
assertAccumState contract inst p nm =
    flip postMapM (Folds.instanceAccumState contract inst) $ \w -> do
        let result = p w
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Accumulated state of of" <+> pretty inst <> colon
                , indent 2 (viaShow w)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Assert that the size of a 'Validator' is below
--   the maximum.
reasonable :: Validator -> Integer -> HUnit.Assertion
reasonable = reasonable' putStrLn

reasonable' :: (String -> IO ()) -> Validator -> Integer -> HUnit.Assertion
reasonable' logger (Ledger.unValidatorScript -> s) maxSize = do
    let sz = Ledger.scriptSize s
        msg = "Script too big! Max. size: " <> show maxSize <> ". Actual size: " <> show sz
    -- so the actual size is visible in the log
    liftIO $ logger ("Script size: " ++ show sz)
    HUnit.assertBool msg (sz <= maxSize)

-- | Compare a golden PIR file to the provided 'CompiledCode'.
goldenPir :: FilePath -> CompiledCode a -> TestTree
goldenPir path code = goldenVsString "PIR" path (pure $ fromString $ show $ pretty $ fromJust $ getPir code)
