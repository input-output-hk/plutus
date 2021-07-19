{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module ErrorHandling where

-- TRIM TO HERE
import           Control.Lens             (makeClassyPrisms, prism', review)
import           Control.Monad            (void)
import           Control.Monad.Error.Lens (catching, throwing, throwing_)
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Data.Default             (Default (def))
import qualified Ledger.TimeSlot          as TimeSlot
import           Playground.Contract
import           Plutus.Contract          (AsContractError (_ContractError), ContractError, awaitTime, logInfo,
                                           mapError, selectList)
import           Prelude                  (Maybe (..), const, show, ($), (.), (<>))

-- Demonstrates how to deal with errors in Plutus contracts. We define a custom
-- error type 'MyError' with three constructors and use
-- 'Control.Lens.makeClassyPrisms' to generate the 'AsMyError' class. We can
-- then use 'MyError' in our contracts with the combinators from
-- 'Control.Monad.Error.Lens'. The unit tests in 'Spec.ErrorHandling' show how
-- to write tests for error conditions.

type Schema =
    Endpoint "throwError" Text
     .\/ Endpoint "catchError" Text
     .\/ Endpoint "catchContractError" ()

-- | 'MyError' has a constructor for each type of error that our contract
 --   can throw. The 'AContractError' constructor wraps a 'ContractError'.
data MyError =
    Error1 Text
    | Error2
    | AContractError ContractError
    deriving Show

makeClassyPrisms ''MyError

instance AsContractError MyError where
    -- 'ContractError' is another error type. It is defined in
    -- 'Plutus.Contract.Request'. By making 'MyError' an
    -- instance of 'AsContractError' we can handle 'ContractError's
    -- thrown by other contracts in our code (see 'catchContractError')
    _ContractError = _AContractError

instance AsMyError Text where
    _MyError = prism' (T.pack . show) (const Nothing)

-- | Throw an 'Error1', using 'Control.Monad.Error.Lens.throwing' and the
--   prism generated by 'makeClassyPrisms'
throw :: AsMyError e => Text -> Contract () s e ()
throw e = do
    logInfo @Text $  "throwError: " <> e
    throwing _Error1 e

-- | Handle the error from 'throw' using 'Control.Monad.Error.Lens.catching'
throwAndCatch :: AsMyError e => Text -> Contract () s e ()
throwAndCatch e =
    let handleError1 :: Text -> Contract () s e ()
        handleError1 t = logInfo $ "handleError: " <> t
     in catching _Error1 (throw e) handleError1

-- | Handle an error from 'awaitTime by wrapping it in the 'AContractError'
--   constructor
catchContractError :: (AsMyError e) => Contract () s e ()
catchContractError =
    catching _AContractError
        (void $ mapError (review _AContractError) $ awaitTime $ TimeSlot.slotToBeginPOSIXTime def 10)
        (\_ -> throwing_ _Error2)

contract
    :: ( AsMyError e
       , AsContractError e
       )
    => Contract () Schema e ()
contract = selectList
    [ endpoint @"throwError" throw
    , endpoint @"catchError" throwAndCatch
    , endpoint @"catchContractError" $ const catchContractError
    ]

endpoints :: (AsMyError e, AsContractError e) => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
