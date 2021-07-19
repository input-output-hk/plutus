{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

{-
This module and its submodules define the Plutus part of a credential
management dapp such as Atala PRISM / Mirror.

There are two actors:

* Mirror. Issues and revokes the on-chain credential tokens.
* User. Uses credentials in transactions

We have the following modules.

1. .Credential: Defines the 'Credential' type and a minting policy script
   for creating and destroying credential tokens
2. .StateMachine: Defines the state machine script that allows Users to
   present their credentials in transactions and Mirror to revoke/destroy
   credentials.
3. .STO: A simple Security Token Offering (STO) that requires a credential
   to be presented by anyone who wants to participate. This is provided for
   testing and demo purposes
4. .CredentialManager: A Plutus application managing all the credentials
   that are available to the User.
5. .Mirror: A Plutus application that locks credentials in state machines
   and revokes them when necessary.
6. .Unlock: Two Plutus applications that each present a credential to unlock
   some funds.

We work with two different script hashes: The hash of the minting policy that
mints the tokens (see 'policy'), and the hash of the state machine instance
that locks a specific credential token for a specific user, identified by their
public key address.

-}
module Plutus.Contracts.Prism(
    -- * Unlock (STO)
    STOSubscriber(..)
    , STOSubscriberSchema
    , UnlockError(..)
    , subscribeSTO
    -- * Unlock (exchange)
    , UnlockExchangeSchema
    , unlockExchange
    -- * Mirror app
    , MirrorSchema
    , CredentialOwnerReference(..)
    , MirrorError(..)
    , mirror
    -- * Credential manager app
    , Credential(..)
    , UserCredential(..)
    , CredentialAuthority(..)
    -- * all-in-one
    , Role(..)
    , PrismSchema
    , PrismError(..)
    , contract
    ) where

import           Control.Lens
import           Data.Aeson                          (FromJSON, ToJSON)
import           GHC.Generics                        (Generic)
import           Plutus.Contracts.Prism.Credential
import           Plutus.Contracts.Prism.Mirror
import           Plutus.Contracts.Prism.StateMachine
import           Plutus.Contracts.Prism.Unlock

import           Plutus.Contract

-- | The roles that we pass to 'contract'.
data Role
    = Mirror -- ^ The 'mirror' contract
    | UnlockSTO -- ^ The 'subscribeSTO' contract
    | UnlockExchange -- ^ The 'unlockExchange' contract
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

type PrismSchema =
    MirrorSchema
    .\/ STOSubscriberSchema
    .\/ UnlockExchangeSchema
    .\/ Endpoint "role" Role

data PrismError =
    UnlockSTOErr UnlockError
    | MirrorErr MirrorError
    | EPError ContractError
    | UnlockExchangeErr UnlockError
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PrismError

instance AsContractError PrismError where
    _ContractError = _EPError . _ContractError

-- | A wrapper around the four prism contracts. This is just a workaround
--   for the emulator, where we can only ever run a single 'Contract'. In
--   the PAB we could simply start all four contracts (credentialManager,
--   mirror, subscribeSTO, subscribeExchange) separately.
contract :: Contract () PrismSchema PrismError ()
contract = fmap getWaited $ endpoint @"role" $ \r -> do
    case r of
        Mirror         -> mapError MirrorErr mirror
        UnlockSTO      -> mapError UnlockSTOErr subscribeSTO
        UnlockExchange -> mapError UnlockExchangeErr unlockExchange
