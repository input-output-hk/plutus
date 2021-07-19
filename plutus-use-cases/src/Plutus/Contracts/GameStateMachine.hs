{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
-- | A guessing game that
--
--   * Uses a state machine to keep track of the current secret word
--   * Uses a token to keep track of who is allowed to make a guess
--

module Plutus.Contracts.GameStateMachine(
    contract
    , typedValidator
    , GameToken
    , mkValidator
    , mintingPolicy
    , LockArgs(..)
    , GuessArgs(..)
    , GameStateMachineSchema, GameError
    , token
    ) where

import           Control.Lens                 (makeClassyPrisms)
import           Control.Monad                (void)
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (to)
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import qualified Ledger.Value                 as V
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check)
import           Schema                       (ToArgument, ToSchema)

import qualified Data.ByteString.Char8        as C

import           Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM

import           Plutus.Contract

import qualified Prelude                      as Haskell

newtype HashedString = HashedString ByteString
    deriving newtype (PlutusTx.IsData, Eq)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString
    deriving newtype (PlutusTx.IsData, Eq)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ClearString

-- | Arguments for the @"lock"@ endpoint
data LockArgs =
    LockArgs
        { lockArgsSecret :: Haskell.String
        -- ^ The secret
        , lockArgsValue  :: Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsOldSecret     :: Haskell.String
        -- ^ The guess
        , guessArgsNewSecret     :: Haskell.String
        -- ^ The new secret
        , guessArgsValueTakenOut :: Value
        -- ^ How much to extract from the contract
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

-- | The schema of the contract. It consists of the two endpoints @"lock"@
--   and @"guess"@ with their respective argument types.
type GameStateMachineSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "guess" GuessArgs

data GameError =
    GameContractError ContractError
    | GameSMError SM.SMContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''GameError

instance AsContractError GameError where
    _ContractError = _GameContractError . _ContractError

instance SM.AsSMContractError GameError where
    _SMContractError = _GameSMError . SM._SMContractError

-- | Top-level contract, exposing both endpoints.
contract :: Contract () GameStateMachineSchema GameError ()
contract = selectList [lock, guess] >> contract

-- | The token that represents the right to make a guess
newtype GameToken = GameToken { unGameToken :: Value }
    deriving newtype (Eq, Haskell.Show)

token :: MintingPolicyHash -> TokenName -> Value
token mps tn = V.singleton (V.mpsSymbol mps) tn 1

-- | State of the guessing game
data GameState =
    Initialised MintingPolicyHash TokenName HashedString
    -- ^ Initial state. In this state only the 'MintTokens' action is allowed.
    | Locked MintingPolicyHash TokenName HashedString
    -- ^ Funds have been locked. In this state only the 'Guess' action is
    --   allowed.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq GameState where
    {-# INLINABLE (==) #-}
    (Initialised sym tn s) == (Initialised sym' tn' s') = sym == sym' && s == s' && tn == tn'
    (Locked sym tn s) == (Locked sym' tn' s')           = sym == sym' && s == s' && tn == tn'
    _ == _                                              = traceIfFalse "states not equal" False

-- | Check whether a 'ClearString' is the preimage of a
--   'HashedString'
checkGuess :: HashedString -> ClearString -> Bool
checkGuess (HashedString actual) (ClearString gss) = actual == (sha2_256 gss)

-- | Inputs (actions)
data GameInput =
      MintToken
    -- ^ Mint the "guess" token
    | Guess ClearString HashedString Value
    -- ^ Make a guess, extract the funds, and lock the remaining funds using a
    --   new secret word.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-# INLINABLE transition #-}
transition :: State GameState -> GameInput -> Maybe (TxConstraints Void Void, State GameState)
transition State{stateData=oldData, stateValue=oldValue} input = case (oldData, input) of
    (Initialised mph tn s, MintToken) ->
        let constraints = Constraints.mustMintCurrency mph tn 1 in
        Just ( constraints
             , State
                { stateData = Locked mph tn s
                , stateValue = oldValue
                }
             )
    (Locked mph tn currentSecret, Guess theGuess nextSecret takenOut)
        | checkGuess currentSecret theGuess ->
        let constraints = Constraints.mustSpendAtLeast (token mph tn) <> Constraints.mustMintCurrency mph tn 0 in
        Just ( constraints
             , State
                { stateData = Locked mph tn nextSecret
                , stateValue = oldValue - takenOut
                }
             )
    _ -> Nothing

type GameStateMachine = SM.StateMachine GameState GameInput

{-# INLINABLE machine #-}
machine :: GameStateMachine
machine = SM.mkStateMachine Nothing transition isFinal where
    isFinal _ = False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType GameStateMachine
mkValidator = SM.mkValidator machine

typedValidator :: Scripts.TypedValidator GameStateMachine
typedValidator = Scripts.mkTypedValidator @GameStateMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

mintingPolicy :: Scripts.MintingPolicy
mintingPolicy = Scripts.forwardingMintingPolicy typedValidator

client :: SM.StateMachineClient GameState GameInput
client = SM.mkStateMachineClient $ SM.StateMachineInstance machine typedValidator

-- | The @"guess"@ endpoint.
guess :: Contract () GameStateMachineSchema GameError (Waited ())
guess = endpoint @"guess" $ \GuessArgs{guessArgsOldSecret,guessArgsNewSecret, guessArgsValueTakenOut} -> do

    let guessedSecret = ClearString (C.pack guessArgsOldSecret)
        newSecret     = HashedString (sha2_256 (C.pack guessArgsNewSecret))

    void
        $ SM.runStep client
            (Guess guessedSecret newSecret guessArgsValueTakenOut)

lock :: Contract () GameStateMachineSchema GameError (Waited ())
lock = endpoint @"lock" $ \LockArgs{lockArgsSecret, lockArgsValue} -> do
    let secret = HashedString (sha2_256 (C.pack lockArgsSecret))
        sym = Scripts.forwardingMintingPolicyHash typedValidator
    _ <- SM.runInitialise client (Initialised sym "guess" secret) lockArgsValue
    void $ SM.runStep client MintToken

PlutusTx.unstableMakeIsData ''GameState
PlutusTx.makeLift ''GameState
PlutusTx.unstableMakeIsData ''GameInput
PlutusTx.makeLift ''GameInput
PlutusTx.makeLift ''GameToken
PlutusTx.unstableMakeIsData ''GameToken
