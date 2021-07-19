{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-- | Plutus implementation of an account that can be unlocked with a token.
--   Whoever owns the token can spend the outputs locked by the contract.
--   (A suitable token can be created with the 'Plutus.Contracts.Currency'
--   contract, or with 'newAccount' in this module)
module Plutus.Contracts.TokenAccount(
  Account(..)
  -- * Contract functionality
  , pay
  , redeem
  , newAccount
  , balance
  , address
  , accountToken
  , payTx
  , redeemTx
  -- * Endpoints
  , TokenAccountSchema
  , HasTokenAccountSchema
  , tokenAccountContract
  -- * Etc.
  , TokenAccount
  , TokenAccountError(..)
  , AsTokenAccountError(..)
  , validatorHash
  , typedValidator
  ) where

import           Control.Lens
import           Control.Monad                    (void)
import           Control.Monad.Error.Lens
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Map                         as Map
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           GHC.Generics                     (Generic)

import           Plutus.Contract
import           Plutus.Contract.Constraints
import qualified PlutusTx                         as PlutusTx

import           Ledger                           (Address, PubKeyHash, Tx, TxOutTx (..), ValidatorHash)
import qualified Ledger                           as Ledger
import qualified Ledger.Constraints               as Constraints
import qualified Ledger.Contexts                  as V
import qualified Ledger.Scripts
import           Ledger.Typed.Scripts             (ValidatorTypes (..))
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (TokenName, Value)
import qualified Ledger.Value                     as Value
import qualified Plutus.Contract.Typed.Tx         as TypedTx

import qualified Plutus.Contracts.Currency        as Currency

newtype Account = Account { accountOwner :: Value.AssetClass }
    deriving stock    (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving Pretty via (PrettyShow Account)

data TokenAccount

instance ValidatorTypes TokenAccount where
    type RedeemerType TokenAccount = ()
    type DatumType TokenAccount = ()

type TokenAccountSchema =
        Endpoint "redeem" (Account, PubKeyHash)
        .\/ Endpoint "pay" (Account, Value)
        .\/ Endpoint "new-account" (TokenName, PubKeyHash)

type HasTokenAccountSchema s =
    ( HasEndpoint "redeem" (Account, PubKeyHash) s
    , HasEndpoint "pay" (Account, Value) s
    , HasEndpoint "new-account" (TokenName, PubKeyHash) s
    )

data TokenAccountError =
    TAContractError ContractError
    | TACurrencyError Currency.CurrencyError
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''TokenAccountError

instance AsContractError TokenAccountError where
    _ContractError = _TAContractError

instance Currency.AsCurrencyError TokenAccountError where
    _CurrencyError = _TACurrencyError

-- | 'redeem', 'pay' and 'newAccount' with endpoints.
tokenAccountContract
    :: forall w s e.
       ( HasTokenAccountSchema s
       , AsTokenAccountError e
       )
    => Contract w s e ()
tokenAccountContract = mapError (review _TokenAccountError) (selectList [redeem_, pay_, newAccount_]) where
    redeem_ = endpoint @"redeem" @(Account, PubKeyHash) @w @s $ \(accountOwner, destination) -> do
        void $ redeem destination accountOwner
        tokenAccountContract
    pay_ = endpoint @"pay" @_ @w @s $ \(accountOwner, value) -> do
        void $ pay accountOwner value
        tokenAccountContract
    newAccount_ = endpoint @"new-account" @_ @w @s $ \(tokenName, initialOwner) -> do
        void $ newAccount tokenName initialOwner
        tokenAccountContract

{-# INLINEABLE accountToken #-}
accountToken :: Account -> Value
accountToken (Account currency) = Value.assetClassValue currency 1

{-# INLINEABLE validate #-}
validate :: Account -> () -> () -> V.ScriptContext -> Bool
validate account _ _ ptx = V.valueSpent (V.scriptContextTxInfo ptx) `Value.geq` accountToken account

typedValidator :: Account -> Scripts.TypedValidator TokenAccount
typedValidator = Scripts.mkTypedValidatorParam @TokenAccount
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

address :: Account -> Address
address = Scripts.validatorAddress . typedValidator

validatorHash :: Account -> ValidatorHash
validatorHash = Ledger.Scripts.validatorHash . Scripts.validatorScript . typedValidator

-- | A transaction that pays the given value to the account
payTx
    ::
    Value
    -> TxConstraints (Scripts.RedeemerType TokenAccount) (Scripts.DatumType TokenAccount)
payTx vl = Constraints.mustPayToTheScript () vl

-- | Pay some money to the given token account
pay
    :: ( AsTokenAccountError e
       )
    => Account
    -> Value
    -> Contract w s e Tx
pay account vl = do
    let inst = typedValidator account
    logInfo @String
        $ "TokenAccount.pay: Paying "
        <> show vl
        <> " into "
        <> show account
    mapError (review _TAContractError)
        $ submitTxConstraints inst
        $ payTx vl

-- | Create a transaction that spends all outputs belonging to the 'Account'.
redeemTx :: forall w s e.
    ( AsTokenAccountError e
    )
    => Account
    -> PubKeyHash
    -> Contract w s e (TxConstraints () (), ScriptLookups TokenAccount)
redeemTx account pk = mapError (review _TAContractError) $ do
    let inst = typedValidator account
    utxos <- utxoAt (address account)
    let totalVal = foldMap (V.txOutValue . txOutTxOut) utxos
        numInputs = Map.size utxos
    logInfo @String
        $ "TokenAccount.redeemTx: Redeeming "
            <> show numInputs
            <> " outputs with a total value of "
            <> show totalVal
    let constraints = TypedTx.collectFromScript utxos ()
                <> Constraints.mustPayToPubKey pk (accountToken account)
        lookups = Constraints.typedValidatorLookups inst
                <> Constraints.unspentOutputs utxos
    -- TODO. Replace 'PubKey' with a more general 'Address' type of output?
    --       Or perhaps add a field 'requiredTokens' to 'LedgerTxConstraints' and let the
    --       balancing mechanism take care of providing the token.
    pure (constraints, lookups)

-- | Empty the account by spending all outputs belonging to the 'Account'.
redeem
  :: ( AsTokenAccountError e
     )
  => PubKeyHash
  -- ^ Where the token should go after the transaction
  -> Account
  -- ^ The token account
  -> Contract w s e Tx
redeem pk account = mapError (review _TokenAccountError) $ do
    (constraints, lookups) <- redeemTx account pk
    tx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups constraints)
    submitUnbalancedTx tx

-- | @balance account@ returns the value of all unspent outputs that can be
--   unlocked with @accountToken account@
balance
    :: ( AsTokenAccountError e
       )
    => Account
    -> Contract w s e Value
balance account = mapError (review _TAContractError) $ do
    utxos <- utxoAt (address account)
    let inner =
            foldMap (view Ledger.outValue . Ledger.txOutTxOut)
            $ utxos
    pure inner

-- | Create a new token and return its 'Account' information.
newAccount
    :: forall w s e.
    (AsTokenAccountError e)
    => TokenName
    -- ^ Name of the token
    -> PubKeyHash
    -- ^ Public key of the token's initial owner
    -> Contract w s e Account
newAccount tokenName pk = mapError (review _TokenAccountError) $ do
    cur <- Currency.mintContract pk [(tokenName, 1)]
    let sym = Currency.currencySymbol cur
    pure $ Account $ Value.assetClass sym tokenName

PlutusTx.makeLift ''Account
PlutusTx.unstableMakeIsData ''Account
