{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
module Plutus.Contract.Types(
    ContractEffs
    , handleContractEffs
    , Contract(..)
    -- * Select
    , select
    , selectEither
    , selectList
    -- * Error handling
    , ContractError(..)
    , AsContractError(..)
    , MatchingError(..)
    , mapError
    , throwError
    , runError
    , handleError
    -- * Checkpoints
    , AsCheckpointError(..)
    , CheckpointError(..)
    , checkpoint
    , checkpointLoop
    -- * Run and update
    , runResumable
    , insertAndUpdate
    , runWithRecord
    -- * State
    , ResumableResult(..)
    , responses
    , requests
    , finalState
    , logs
    , lastState
    , checkpointStore
    , observableState
    , shrinkResumableResult
    -- * Run with continuations
    , SuspendedContract(..)
    , resumableResult
    , continuations
    , checkpointKey
    , suspend
    , runStep
    , lastLogs
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except              (MonadError (..))
import           Control.Monad.Freer
import           Control.Monad.Freer.Error         (Error)
import qualified Control.Monad.Freer.Error         as E
import           Control.Monad.Freer.Extras.Log    (LogMessage, LogMsg, handleLogIgnore, handleLogWriter)
import           Control.Monad.Freer.Extras.Modify (raiseEnd, raiseUnderN, writeIntoState)
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer        (Writer)
import qualified Control.Monad.Freer.Writer        as W
import           Data.Aeson                        (Value)
import qualified Data.Aeson                        as Aeson
import           Data.Foldable                     (foldl')
import qualified Data.IntervalSet                  as IS
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Row                          (Row)
import           Data.Sequence                     (Seq)
import           GHC.Generics                      (Generic)

import           Plutus.Contract.Checkpoint        (AsCheckpointError (..), Checkpoint (..), CheckpointError (..),
                                                    CheckpointKey, CheckpointLogMsg, CheckpointStore,
                                                    completedIntervals, handleCheckpoint, jsonCheckpoint,
                                                    jsonCheckpointLoop)
import           Plutus.Contract.Resumable         hiding (responses, select)
import qualified Plutus.Contract.Resumable         as Resumable

import           Plutus.Contract.Effects           (PABReq, PABResp, Waited (..))
import qualified PlutusTx.Applicative              as PlutusTx
import qualified PlutusTx.Functor                  as PlutusTx
import           Prelude                           as Haskell
import           Wallet.Types                      (AsContractError (..), ContractError (..), MatchingError (..))

-- | Effects that are available to contracts.
type ContractEffs w e =
    '[ Error e
    ,  LogMsg Value
    ,  Writer w
    ,  Checkpoint
    ,  Resumable PABResp PABReq
    ]

type ContractEnv = (IterationID, RequestID)

newtype AccumState w = AccumState { unAccumState :: w }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid, Aeson.ToJSON, Aeson.FromJSON)

_AccumState :: forall w. Iso' (AccumState w) w
_AccumState = iso unAccumState AccumState

handleContractEffs ::
  forall w e effs a.
  ( Member (Error e) effs
  , Member (State CheckpointStore) effs
  , Member (State CheckpointKey) effs
  , Member (State (AccumState w)) effs
  , Member (LogMsg CheckpointLogMsg) effs
  , Member (LogMsg Value) effs
  , Monoid w
  )
  => Eff (ContractEffs w e) a
  -> Eff effs (Maybe (MultiRequestContStatus PABResp PABReq effs a))
handleContractEffs =
  suspendNonDet @PABResp @PABReq @a @effs
  . handleResumable @PABResp @PABReq
  . handleCheckpoint
  . addEnvToCheckpoint
  . interpret @(Writer w) (writeIntoState _AccumState)
  . subsume @(LogMsg Value)
  . subsume @(Error e)
  . raiseEnd

getContractEnv ::
  forall effs.
  ( Member (State RequestID) effs
  , Member (State IterationID) effs
  )
  => Eff effs ContractEnv
getContractEnv = (,) <$> get <*> get

putContractEnv ::
  forall effs.
  ( Member (State RequestID) effs
  , Member (State IterationID) effs
  )
  => ContractEnv
  -> Eff effs ()
putContractEnv (it, req) = put it >> put req

addEnvToCheckpoint ::
  forall effs.
  ( Member (State RequestID) effs
  , Member (State IterationID) effs
  )
  => Eff (Checkpoint ': effs)
  ~> Eff (Checkpoint ': effs)
addEnvToCheckpoint = reinterpret @Checkpoint @Checkpoint @effs $ \case
  DoCheckpoint -> send DoCheckpoint
  AllocateKey -> send AllocateKey
  Store k k' a -> do
    env <- getContractEnv
    send $ Store k k' (env, a)
  Retrieve k -> do
    result <- send $ Retrieve @(ContractEnv, _) k
    case result of
      Right (Just (env, a)) -> do
        putContractEnv env
        pure (Right (Just a))
      Left err -> do
        pure (Left err)
      Right Nothing -> do
        pure (Right Nothing)

-- | @Contract w s e a@ is a contract with schema 's', producing a value of
--  type 'a' or an error 'e'. See note [Contract Schema].
--
newtype Contract w (s :: Row *) e a = Contract { unContract :: Eff (ContractEffs w e) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadError e (Contract w s e) where
    throwError = Contract . E.throwError
    catchError (Contract f) handler =
      Contract
      $ E.catchError f
      $ unContract . handler

instance PlutusTx.Functor (Contract w s e) where
  fmap = Haskell.fmap

instance PlutusTx.Applicative (Contract w s e) where
  (<*>) = (Haskell.<*>)
  pure  = Haskell.pure

instance Bifunctor (Contract w s) where
  bimap l r = mapError l . fmap r

-- | @select@ returns the contract that makes progress first, discarding the
--   other one.
select :: forall w s e a. Contract w s e (Waited a) -> Contract w s e (Waited a) -> Contract w s e (Waited a)
select (Contract l) (Contract r) = Contract (Resumable.select @PABResp @PABReq @(ContractEffs w e) l r)

-- | A variant of @select@ for contracts with different return types.
selectEither :: forall w s e a b. Contract w s e (Waited a) -> Contract w s e (Waited b) -> Contract w s e (Waited (Either a b))
selectEither l r = (fmap Left <$> l) `select` (fmap Right <$> r)

selectList :: [Contract w s e (Waited ())] -> Contract w s e ()
selectList [] = pure ()
selectList cs = getWaited <$> foldr1 select cs

-- | Write the current state of the contract to a checkpoint.
checkpoint :: forall w s e a. (AsCheckpointError e, Aeson.FromJSON a, Aeson.ToJSON a) => Contract w s e a -> Contract w s e a
checkpoint = Contract . jsonCheckpoint @e . unContract

checkpointLoop :: forall w s e a b. (AsCheckpointError e, Aeson.FromJSON a, Aeson.ToJSON a, Aeson.ToJSON b, Aeson.FromJSON b) => (a -> Contract w s e (Either b a)) -> a -> Contract w s e b
checkpointLoop f initial = Contract $ jsonCheckpointLoop @e (fmap unContract f) initial

-- | Transform any exceptions thrown by the 'Contract' using the given function.
mapError ::
  forall w s e e' a.
  (e -> e')
  -> Contract w s e a
  -> Contract w s e' a
mapError f = handleError (throwError . f)

-- | Turn a contract with error type 'e' and return type 'a' into one with
--   any error type (ie. throwing no errors) that returns 'Either e a'
runError ::
  forall w s e e0 a.
  Contract w s e a
  -> Contract w s e0 (Either e a)
runError (Contract r) = Contract (E.runError $ raiseUnderN @'[E.Error e0] r)

-- | Handle errors, potentially throwing new errors.
handleError ::
  forall w s e e' a.
  (e -> Contract w s e' a)
  -> Contract w s e a
  -> Contract w s e' a
handleError f (Contract c) = Contract c' where
  c' = E.handleError @e (raiseUnderN @'[E.Error e'] c) (fmap unContract f)

type SuspendedContractEffects w e =
  Error e
  ': State CheckpointKey
  ': State CheckpointStore
  ': LogMsg CheckpointLogMsg
  ': State (AccumState w)
  ': LogMsg Value
  ': Writer (Seq (LogMessage Value))
  ': '[]

-- | The result of running a 'Resumable'
data ResumableResult w e i o a =
    ResumableResult
        { _responses       :: Responses (CheckpointKey, i) -- The record with the resumable's execution history
        , _requests        :: Requests o -- Handlers that the 'Resumable' has registered
        , _finalState      :: Either e (Maybe a) -- Error or final state of the 'Resumable' (if it has finished)
        , _logs            :: Seq (LogMessage Value) -- All log messages that have been produced by this instance.
        , _lastLogs        :: Seq (LogMessage Value) -- Log messages produced in the last step
        , _checkpointStore :: CheckpointStore
        , _observableState :: w -- ^ Accumulated, observable state of the contract
        , _lastState       :: w -- ^ Last accumulated state
        }
        deriving stock (Generic, Show)
        deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

makeLenses ''ResumableResult

-- | Shrink the 'ResumableResult' by deleting everything that's not needed to restore the
--   state of the contract instance.
shrinkResumableResult :: ResumableResult w e i o a -> ResumableResult w e i o a
shrinkResumableResult rs =
  let comp = rs ^. checkpointStore . to completedIntervals
      isCovered :: CheckpointKey -> Bool
      isCovered k = not $ IS.null $ IS.containing comp k
  in rs & logs .~ mempty
        & over (responses . _Responses) (Map.filter (not . isCovered . fst))

data SuspendedContract w e i o a =
  SuspendedContract
    { _resumableResult :: ResumableResult w e i o a
    , _continuations   :: Maybe (MultiRequestContStatus i o (SuspendedContractEffects w e) a)
    , _checkpointKey   :: CheckpointKey
    }

makeLenses ''SuspendedContract

runResumable ::
  Monoid w
  => [Response PABResp]
  -> CheckpointStore
  -> Eff (ContractEffs w e) a
  -> ResumableResult w e PABResp PABReq a
runResumable events store action =
  let initial = suspend store action
      runStep' con rsp = fromMaybe con (runStep con rsp)
      result = foldl' runStep' initial events & view resumableResult
  in result

runWithRecord ::
  forall w e a.
  Monoid w
  => Eff (ContractEffs w e) a
  -> CheckpointStore
  -> Responses PABResp
  -> ResumableResult w e PABResp PABReq a
runWithRecord action store events =
  runResumable (Resumable.responses events) store action

mkResult ::
  forall w e a.
  Monoid w
  => w
  -> Seq (LogMessage Value) -- ^ Old logs
  -> ( Either e (Maybe (MultiRequestContStatus PABResp PABReq (SuspendedContractEffects w e) a))
     , CheckpointKey
     , CheckpointStore
     , AccumState w
     , Seq (LogMessage Value)
     )
  -> SuspendedContract w e PABResp PABReq a
mkResult oldW oldLogs (initialRes, cpKey, cpStore, AccumState newW, newLogs) =
  SuspendedContract
      { _resumableResult =
          ResumableResult
            { _responses = mempty
            , _requests =
                let getRequests = \case { AContinuation MultiRequestContinuation{ndcRequests} -> Just ndcRequests; _ -> Nothing }
                in either mempty ((fromMaybe mempty) . (>>= getRequests)) initialRes
            , _finalState =
                let getResult = \case { AResult a -> Just a; _ -> Nothing } in
                fmap (>>= getResult) initialRes
            , _logs = oldLogs <> newLogs
            , _lastLogs = newLogs
            , _checkpointStore = cpStore
            , _observableState = oldW <> newW
            , _lastState = newW
            }
      , _continuations = either (const Nothing) id initialRes
      , _checkpointKey = cpKey
      }

runSuspContractEffects ::
  forall w e a.
  Monoid w
  => CheckpointKey
  -> CheckpointStore
  -> Eff (SuspendedContractEffects w e) a
  -> (Either e a, CheckpointKey, CheckpointStore, AccumState w, Seq (LogMessage Value))
runSuspContractEffects cpKey cpStore =
  flatten
    . run
    . W.runWriter @(Seq (LogMessage Value))
    . interpret (handleLogWriter @Value @(Seq (LogMessage Value)) $ unto return)
    . runState @(AccumState w) mempty
    . handleLogIgnore @CheckpointLogMsg
    . runState cpStore
    . runState cpKey
    . E.runError @e where
      flatten ((((e, k), s), w), l) = (e, k, s, w, l)

-- | Run an action of @ContractEffs@ until it requests input for the first
--   time, returning the 'SuspendedContract'
suspend ::
  forall w e a.
  Monoid w
  => CheckpointStore
  -> Eff (ContractEffs w e) a -- ^ The contract
  -> SuspendedContract w e PABResp PABReq a
suspend store action =
  let initialKey = 0 in
  mkResult mempty mempty
    $ runSuspContractEffects @w @e
      initialKey
      store
      (handleContractEffs @w @e @(SuspendedContractEffects w e) action)

-- | Feed a 'Response' to a 'SuspendedContract'.
runStep ::
  forall w e a.
  Monoid w
  => SuspendedContract w e PABResp PABReq a
  -> Response PABResp
  -> Maybe (SuspendedContract w e PABResp PABReq a)
runStep SuspendedContract{_continuations=Just (AContinuation MultiRequestContinuation{ndcCont}), _checkpointKey, _resumableResult=ResumableResult{_responses, _checkpointStore, _observableState=oldW, _logs=oldLogs}} event =
  Just
    $ set (resumableResult . responses) (insertResponse (fmap (_checkpointKey,) event) _responses)
    $ mkResult oldW oldLogs
    $ runSuspContractEffects
        _checkpointKey
        _checkpointStore
        $ ndcCont event
runStep _ _ = Nothing

insertAndUpdate ::
  forall w e a.
  Monoid w
  => Eff (ContractEffs w e) a
  -> CheckpointStore -- ^ Checkpoint store
  -> Responses (CheckpointKey, PABResp)  -- ^ Previous responses
  -> Response PABResp
  -> ResumableResult w e PABResp PABReq a
insertAndUpdate action store record newResponse =
  runWithRecord action store (insertResponse newResponse $ fmap snd record)
