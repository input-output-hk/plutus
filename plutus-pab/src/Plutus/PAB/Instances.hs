{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- that's what this module is all about
module Plutus.PAB.Instances() where

import           Cardano.BM.Data.Tracer        (ToObject (..), TracingVerbosity (..))
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           Servant.Client                (ClientError)

import           Cardano.BM.Data.Tracer.Extras (StructuredLog, Tagged (..), mkObjectStr)
import           Control.Monad.Logger          (Loc, LogSource)
import           Wallet.Emulator.Wallet        (WalletEvent (..))

deriving via (Tagged "location" Loc) instance StructuredLog Loc
deriving via (Tagged "source" LogSource) instance StructuredLog LogSource

instance ToObject WalletEvent where
    toObject v = \case
        GenericLog t ->
            mkObjectStr "generic log" (Tagged @"message" t)
        CheckpointLog msg ->
            mkObjectStr "checkpoint log" msg
        RequestHandlerLog msg ->
            mkObjectStr "request handler log" msg
        TxBalanceLog msg ->
            mkObjectStr "tx balance log" $
                case v of
                    MaximalVerbosity -> Left msg
                    _                -> Right ()

deriving instance ToJSON Loc
deriving instance FromJSON Loc

-- 'ClientError' is not a simple type. Adding orphan 'ToJSON'/'FromJSON'
-- instances for it would require about 10 standalone deriving clauses,
-- plus a number of libraries added to 'build-depends'.
--
-- I also believe that the chances of the instances actually being needed
-- are low. Therefore I went with the two incorrect instances below.

instance FromJSON ClientError where
    parseJSON _ = fail "FromJSON ClientError: Not Implemented"

instance ToJSON ClientError where
    toJSON = toJSON . show

