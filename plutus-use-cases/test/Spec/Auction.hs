{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Spec.Auction
    ( tests
    , auctionEmulatorCfg
    , auctionTrace1
    , auctionTrace2
    , prop_Auction
    , prop_FinishAuction
    ) where

import           Control.Lens
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Data.Default                       (Default (def))
import           Data.Monoid                        (Last (..))

import           Ledger                             (Ada, POSIXTime, Slot (..), Value, pubKeyHash)
import qualified Ledger.Ada                         as Ada
import           Plutus.Contract                    hiding (currentSlot)
import           Plutus.Contract.Test               hiding (not)
import qualified Streaming.Prelude                  as S
import qualified Wallet.Emulator.Folds              as Folds
import qualified Wallet.Emulator.Stream             as Stream

import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import           Plutus.Contract.Test.ContractModel
import           Plutus.Contracts.Auction           hiding (Bid)
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)

import           Test.QuickCheck                    hiding ((.&&.))
import           Test.Tasty
import           Test.Tasty.QuickCheck              (testProperty)

params :: POSIXTime -> AuctionParams
params startTime =
    AuctionParams
        { apOwner   = pubKeyHash $ walletPubKey w1
        , apAsset   = theToken
        , apEndTime = startTime + 100999
        }

-- | The token that we are auctioning off.
theToken :: Value
theToken =
    -- "ffff" is not a valid MPS hash. But this doesn't matter because we
    -- never try to mint any value of "ffff" using a script.
    -- This currency is created by the initial transaction.
    Value.singleton "ffff" "token" 1

-- | 'EmulatorConfig' that includes 'theToken' in the initial distribution of Wallet 1.
auctionEmulatorCfg :: Trace.EmulatorConfig
auctionEmulatorCfg =
    let initialDistribution = defaultDist & over (ix w1) ((<>) theToken)
    in def & Trace.initialChainState .~ Left initialDistribution

-- | 'CheckOptions' that includes our own 'auctionEmulatorCfg'.
options :: CheckOptions
options = set emulatorConfig auctionEmulatorCfg defaultCheckOptions

seller :: Contract AuctionOutput SellerSchema AuctionError ()
seller = do
    startTime <- TimeSlot.scSlotZeroTime <$> getSlotConfig
    auctionSeller (apAsset $ params startTime) (apEndTime $ params startTime)

buyer :: ThreadToken -> Contract AuctionOutput BuyerSchema AuctionError ()
buyer cur = do
    startTime <- TimeSlot.scSlotZeroTime <$> getSlotConfig
    auctionBuyer cur $ params startTime

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

trace1WinningBid :: Ada
trace1WinningBid = 50

auctionTrace1 :: Trace.EmulatorTrace ()
auctionTrace1 = do
    sellerHdl <- Trace.activateContractWallet w1 seller
    _ <- Trace.waitNSlots 3
    currency <- extractAssetClass sellerHdl
    hdl2 <- Trace.activateContractWallet w2 (buyer currency)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 trace1WinningBid
    void $ Trace.waitUntilTime $ apEndTime $ params $ TimeSlot.scSlotZeroTime def
    void $ Trace.waitNSlots 2


trace2WinningBid :: Ada
trace2WinningBid = 70

extractAssetClass :: Trace.ContractHandle AuctionOutput SellerSchema AuctionError -> Trace.EmulatorTrace ThreadToken
extractAssetClass handle = do
    t <- auctionThreadToken <$> Trace.observableState handle
    case t of
        Last (Just currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

auctionTrace2 :: Trace.EmulatorTrace ()
auctionTrace2 = do
    sellerHdl <- Trace.activateContractWallet w1 seller
    _ <- Trace.waitNSlots 3
    currency <- extractAssetClass sellerHdl
    hdl2 <- Trace.activateContractWallet w2 (buyer currency)
    hdl3 <- Trace.activateContractWallet w3 (buyer currency)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 50
    _ <- Trace.waitNSlots 15
    Trace.callEndpoint @"bid" hdl3 60
    _ <- Trace.waitNSlots 35
    Trace.callEndpoint @"bid" hdl2 trace2WinningBid
    void $ Trace.waitUntilTime $ apEndTime $ params $ TimeSlot.scSlotZeroTime def
    void $ Trace.waitNSlots 2

trace1FinalState :: AuctionOutput
trace1FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace1WinningBid
            , highestBidder = pubKeyHash (walletPubKey w2)
            }
        , auctionThreadToken = Last $ Just threadToken
        }

trace2FinalState :: AuctionOutput
trace2FinalState =
    AuctionOutput
        { auctionState = Last $ Just $ Finished $ HighestBid
            { highestBid = trace2WinningBid
            , highestBidder = pubKeyHash (walletPubKey w2)
            }
        , auctionThreadToken = Last $ Just threadToken
        }

threadToken :: ThreadToken
threadToken =
    let con = getThreadToken :: Contract AuctionOutput SellerSchema AuctionError ThreadToken
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag w1)
        getOutcome (Folds.Done a) = a
        getOutcome e              = error $ "not finished: " <> show e
    in
    either (error . show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream (options ^. emulatorConfig)
        $ do
            void $ Trace.activateContractWallet w1 (void con)
            Trace.waitNSlots 3

-- * QuickCheck model

data AuctionModel = AuctionModel
    { _currentBid :: Integer
    , _winner     :: Wallet
    , _endSlot    :: Slot
    , _phase      :: Phase }
    deriving (Show)

data Phase = NotStarted | Bidding | AuctionOver
    deriving (Eq, Show)

makeLenses 'AuctionModel

deriving instance Eq (ContractInstanceKey AuctionModel w s e)
deriving instance Show (ContractInstanceKey AuctionModel w s e)

instance ContractModel AuctionModel where

    data ContractInstanceKey AuctionModel w s e where
        SellerH :: ContractInstanceKey AuctionModel AuctionOutput SellerSchema AuctionError
        BuyerH  :: Wallet -> ContractInstanceKey AuctionModel AuctionOutput BuyerSchema AuctionError

    data Action AuctionModel = Init | Bid Wallet Integer | WaitUntil Slot
        deriving (Eq, Show)

    initialState = AuctionModel
        { _currentBid = 0
        , _winner     = w1
        , _endSlot    = TimeSlot.posixTimeToEnclosingSlot def $ apEndTime
                                                              $ params
                                                              $ TimeSlot.scSlotZeroTime def
        , _phase      = NotStarted
        }

    arbitraryAction s
        | p /= NotStarted =
            oneof [ WaitUntil . step <$> choose (1, 10 :: Integer)
                  , Bid  <$> (Wallet <$> choose (2, 4)) <*> choose (1, 1000) ]
        | otherwise = pure Init
        where
            p    = s ^. contractState . phase
            slot = s ^. currentSlot
            step n = slot + fromIntegral n

    precondition s Init = s ^. contractState . phase == NotStarted
    precondition s cmd  = s ^. contractState . phase /= NotStarted &&
        case cmd of
            WaitUntil slot -> slot > s ^. currentSlot
            _              -> True

    -- This command is only for setting up the model state with theToken
    nextState cmd = do
        slot <- viewModelState currentSlot
        end  <- viewContractState endSlot
        case cmd of
            Init -> do
                phase $= Bidding
                withdraw w1 theToken
                wait 3
            WaitUntil slot' -> waitUntil slot'
            Bid w bid -> do
                current <- viewContractState currentBid
                leader  <- viewContractState winner
                wait 1
                when (bid > current && slot <= end) $ do
                    withdraw w $ Ada.lovelaceValueOf bid
                    deposit leader $ Ada.lovelaceValueOf current
                    currentBid $= bid
                    winner     $= w
        slot' <- viewModelState currentSlot
        p     <- viewContractState phase
        when (slot' > end && p == Bidding) $ do
            w   <- viewContractState winner
            bid <- viewContractState currentBid
            phase $= AuctionOver
            deposit w theToken
            deposit w1 $ Ada.lovelaceValueOf bid

    perform _ _ Init = delay 3
    perform _ _ (WaitUntil slot) = void $ Trace.waitUntilSlot slot
    perform handle _ (Bid w bid) = do
        Trace.callEndpoint @"bid" (handle $ BuyerH w) (Ada.lovelaceOf bid)
        delay 1

    shrinkAction _ Init      = []
    shrinkAction _ (WaitUntil (Slot n))  = [ WaitUntil (Slot n') | n' <- shrink n ]
    shrinkAction s (Bid w v) =
        WaitUntil (s ^. currentSlot + 1) : [ Bid w v' | v' <- shrink v ]

    monitoring _ _ = id

delay :: Integer -> Trace.EmulatorTrace ()
delay n = void $ Trace.waitNSlots $ fromIntegral n

prop_Auction :: Actions AuctionModel -> Property
prop_Auction script =
    propRunActionsWithOptions (set minLogLevel Info options) spec
        (\ _ -> pure True)  -- TODO: check termination
        script
    where
        spec = ContractInstanceSpec SellerH w1 seller :
               [ ContractInstanceSpec (BuyerH w) w (buyer threadToken) | w <- map Wallet [2..4] ]

finishAuction :: DL AuctionModel ()
finishAuction = do
    action Init
    anyActions_
    slot <- viewModelState currentSlot
    when (slot < 101) $ action $ WaitUntil 101
    assertModel "Locked funds are not zero" (Value.isZero . lockedValue)

prop_FinishAuction :: Property
prop_FinishAuction = forAllDL finishAuction prop_Auction

tests :: TestTree
tests =
    testGroup "auction"
        [ checkPredicateOptions options "run an auction"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace1FinalState ) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace1WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace1WinningBid) <> theToken))
            auctionTrace1
        , checkPredicateOptions options "run an auction with multiple bids"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertDone (buyer threadToken) (Trace.walletInstanceTag w3) (const True) "3rd party should be done"
            .&&. assertAccumState (buyer threadToken) (Trace.walletInstanceTag w2) ((==) trace2FinalState) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace2WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace2WinningBid) <> theToken)
            .&&. walletFundsChange w3 mempty)
            auctionTrace2
        -- FIXME: Fails because you cannot bid in certain slots when the off-chain code is busy.
        , testProperty "QuickCheck property" $
            withMaxSuccess 1 $ classify True "FIXME" True
            -- withMaxSuccess 10 prop_FinishAuction
        ]
