{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Wallet.Emulator.Chain where

import           Codec.Serialise                (Serialise)
import           Control.DeepSeq                (NFData)
import           Control.Lens                   hiding (index)
import           Control.Monad.Freer
import           Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import           Control.Monad.Freer.State
import qualified Control.Monad.State            as S
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Foldable                  (traverse_)
import           Data.List                      (partition, (\\))
import           Data.Maybe                     (mapMaybe)
import           Data.Text.Prettyprint.Doc
import           Data.Traversable               (for)
import           GHC.Generics                   (Generic)
import           Ledger                         (Block, Blockchain, OnChainTx (..), ScriptValidationEvent, Slot (..),
                                                 Tx (..), TxId, eitherTx, txId)
import qualified Ledger.Index                   as Index
import qualified Ledger.Interval                as Interval
import           Ledger.TimeSlot                (SlotConfig)
import           Plutus.Contract.Util           (uncurry3)

-- | Events produced by the blockchain emulator.
data ChainEvent =
    TxnValidate TxId Tx [ScriptValidationEvent]
    -- ^ A transaction has been validated and added to the blockchain.
    | TxnValidationFail Index.ValidationPhase TxId Tx Index.ValidationError [ScriptValidationEvent]
    -- ^ A transaction failed to validate.
    | SlotAdd Slot
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ChainEvent where
    pretty = \case
        TxnValidate i _ _           -> "TxnValidate" <+> pretty i
        TxnValidationFail p i _ e _ -> "TxnValidationFail" <+> pretty p <+> pretty i <> colon <+> pretty e
        SlotAdd sl                  -> "SlotAdd" <+> pretty sl

-- | A pool of transactions which have yet to be validated.
type TxPool = [Tx]

data ChainState = ChainState {
    _chainNewestFirst :: Blockchain, -- ^ The current chain, with the newest transactions first in the list.
    _txPool           :: TxPool, -- ^ The pool of pending transactions.
    _index            :: Index.UtxoIndex, -- ^ The UTxO index, used for validation.
    _currentSlot      :: Slot -- ^ The current slot number
} deriving (Show, Generic, Serialise, NFData)

emptyChainState :: ChainState
emptyChainState = ChainState [] [] mempty 0

makeLenses ''ChainState

data ChainControlEffect r where
    ProcessBlock :: ChainControlEffect Block
    ModifySlot :: (Slot -> Slot) -> ChainControlEffect Slot

data ChainEffect r where
    QueueTx :: Tx -> ChainEffect ()
    GetCurrentSlot :: ChainEffect Slot
    GetSlotConfig :: ChainEffect SlotConfig

-- | Make a new block
processBlock :: Member ChainControlEffect effs => Eff effs Block
processBlock = send ProcessBlock

-- | Adjust the current slot number, returning the new slot.
modifySlot :: Member ChainControlEffect effs => (Slot -> Slot) -> Eff effs Slot
modifySlot = send . ModifySlot

queueTx :: Member ChainEffect effs => Tx -> Eff effs ()
queueTx tx = send (QueueTx tx)

getSlotConfig :: Member ChainEffect effs => Eff effs SlotConfig
getSlotConfig = send GetSlotConfig

getCurrentSlot :: Member ChainEffect effs => Eff effs Slot
getCurrentSlot = send GetCurrentSlot

type ChainEffs = '[State ChainState, LogMsg ChainEvent]

handleControlChain :: Members ChainEffs effs => ChainControlEffect ~> Eff effs
handleControlChain = \case
    ProcessBlock -> do
        st <- get
        let pool  = st ^. txPool
            slot  = st ^. currentSlot
            idx   = st ^. index
            ValidatedBlock block events rest =
                validateBlock slot idx pool

        let st' = st & txPool .~ rest
                     & addBlock block

        put st'
        traverse_ logEvent events

        pure block
    ModifySlot f -> modify @ChainState (over currentSlot f) >> gets (view currentSlot)

logEvent :: Member (LogMsg ChainEvent) effs => ChainEvent -> Eff effs ()
logEvent e = case e of
    SlotAdd{}           -> logDebug e
    TxnValidationFail{} -> logWarn e
    TxnValidate{}       -> logInfo e

handleChain :: (Members ChainEffs effs) => SlotConfig -> ChainEffect ~> Eff effs
handleChain slotConfig = \case
    QueueTx tx     -> modify $ over txPool (addTxToPool tx)
    GetCurrentSlot -> gets _currentSlot
    GetSlotConfig  -> pure slotConfig

-- | The result of validating a block.
data ValidatedBlock = ValidatedBlock
    { vlbValid  :: Block
    -- ^ The transactions that have been validated in this block.
    , vlbEvents :: [ChainEvent]
    -- ^ Transaction validation events for the transactions in this block.
    , vlbRest   :: [Tx]
    -- ^ The transactions that haven't been validated because the current slot is
    --   not in their validation interval.
    }

-- | Validate a block given the current slot and UTxO index, returning the valid
--   transactions, success/failure events, remaining transactions and the
--   updated UTxO set.
validateBlock :: Slot -> Index.UtxoIndex -> [Tx] -> ValidatedBlock
validateBlock slot@(Slot s) idx txns =
    let
        -- Select those transactions that can be validated in the
        -- current slot
        (eligibleTxns, rest) = partition (canValidateNow slot) txns

        -- Validate eligible transactions, updating the UTXO index each time
        processed =
            flip S.evalState idx $ for eligibleTxns $ \tx -> do
                (err, events_) <- validateEm slot tx
                pure (tx, err, events_)

        -- The new block contains all transaction that were validated
        -- successfully
        block = mapMaybe toOnChain processed
          where
            toOnChain (_ , Just (Index.Phase1, _), _) = Nothing
            toOnChain (tx, Just (Index.Phase2, _), _) = Just (Invalid tx)
            toOnChain (tx, Nothing               , _) = Just (Valid tx)

        -- Also return an `EmulatorEvent` for each transaction that was
        -- processed
        nextSlot = Slot (s + 1)
        events   = (uncurry3 mkValidationEvent <$> processed) ++ [SlotAdd nextSlot]

    in ValidatedBlock block events rest

-- | Check whether the given transaction can be validated in the given slot.
canValidateNow :: Slot -> Tx -> Bool
canValidateNow slot tx = Interval.member slot (txValidRange tx)

mkValidationEvent :: Tx -> Maybe Index.ValidationErrorInPhase -> [ScriptValidationEvent] -> ChainEvent
mkValidationEvent t result events =
    case result of
        Nothing           -> TxnValidate (txId t) t events
        Just (phase, err) -> TxnValidationFail phase (txId t) t err events

-- | Validate a transaction in the current emulator state.
validateEm :: S.MonadState Index.UtxoIndex m => Slot -> Tx -> m (Maybe Index.ValidationErrorInPhase, [ScriptValidationEvent])
validateEm h txn = do
    idx <- S.get
    let ((e, idx'), events) = Index.runValidation (Index.validateTransaction h txn) idx
    _ <- S.put idx'
    pure (e, events)

-- | Adds a block to ChainState, without validation.
addBlock :: Block -> ChainState -> ChainState
addBlock blk st =
  st & chainNewestFirst %~ (blk :)
     & index %~ Index.insertBlock blk
     -- The block update may contain txs that are not in this client's
     -- `txPool` which will get ignored
     & txPool %~ (\\ map (eitherTx id id) blk)

addTxToPool :: Tx -> TxPool -> TxPool
addTxToPool = (:)

makePrisms ''ChainEvent
