{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-

'beam' handler for the 'ContractStore' effect

-}
module Plutus.PAB.Db.Beam.ContractStore
  (handleContractStore)
  where

import           Control.Lens
import           Control.Monad                       (join)
import           Control.Monad.Freer                 (Eff, Member, type (~>))
import           Control.Monad.Freer.Error           (Error, throwError)
import           Control.Monad.Freer.Extras          (LogMsg)
import           Data.Aeson                          (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Builder             (toLazyByteString)
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as LB
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Text.Encoding                  (encodeUtf8Builder)
import qualified Data.Text.Encoding                  as Text
import           Data.UUID                           (fromText, toText)
import           Database.Beam                       hiding (updateRow)
import           Plutus.PAB.Effects.Contract         (ContractStore (..), PABContract (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions (getContract), fromResponse, getResponse)
import           Plutus.PAB.Effects.DbStore          hiding (ContractInstanceId)
import           Plutus.PAB.Monitoring.Monitoring    (PABMultiAgentMsg)
import           Plutus.PAB.Types                    (PABError (..))
import           Plutus.PAB.Webserver.Types          (ContractActivationArgs (..))
import           Wallet.Emulator.Wallet              (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

-- | Convert from the internal representation of a contract into the database
-- representation.
mkRow
  :: (ToJSON a)
  => ContractActivationArgs (ContractDef (Builtin a))
  -> ContractInstanceId
  -> ContractInstance
mkRow ContractActivationArgs{caID, caWallet} instanceId
  = ContractInstance
      (uuidStr instanceId)
      (Text.decodeUtf8 $ B.concat $ LB.toChunks $ encode caID)
      (Text.pack . show . getWallet $ caWallet)
      Nothing -- No state, initially
      True    -- 'Active' immediately

-- | Convert from the database representation of a contract into the
-- internal representation.
mkContracts
  :: forall a.
  ( FromJSON a )
  => [ContractInstance]
  -> Map ContractInstanceId (ContractActivationArgs (ContractDef (Builtin a)))
mkContracts xs =
  Map.fromList xs'
    where
      -- Silently drop those items that failed to decode to UUIDs and contract id
      xs'    = [ (k, v) | Just (k, v) <- map f xs ]
      toId i = ContractInstanceId <$> fromText i
      f ci   = do
          ciId <- toId $ _contractInstanceId ci
          contractId <- decode
                      . toLazyByteString
                      . encodeUtf8Builder
                      . _contractInstanceContractId
                      $ ci
          let wallet = Wallet . read . Text.unpack . _contractInstanceWallet $ ci
          return ( ciId
                 , ContractActivationArgs contractId wallet
                 )

-- | Our database doesn't store UUIDs natively, so we need to convert them
-- from a string.
uuidStr :: ContractInstanceId -> Text
uuidStr = toText . unContractInstanceId

-- | Run the 'ContractStore' actions in the 'DbStore' context.
handleContractStore ::
  forall a effs.
  ( Member DbStoreEffect effs
  , Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin a))) effs
  , ToJSON a
  , FromJSON a
  , HasDefinitions a
  )
  => ContractStore (Builtin a)
  ~> Eff effs
handleContractStore = \case
  PutStartInstance args instanceId ->
    addRow (_contractInstances db)
      $ mkRow args instanceId

  PutState _ instanceId state ->
    let encode' = Just . Text.decodeUtf8 . B.concat . LB.toChunks . encode . getResponse
    in updateRow
        $ update (_contractInstances db)
            (\ci -> ci ^. contractInstanceState <-. val_ (encode' state))
            (\ci -> ci ^. contractInstanceId ==. val_ (uuidStr instanceId))

  GetState instanceId -> do
    let decodeText = decode . toLazyByteString . encodeUtf8Builder
        extractState = \case
          Nothing -> throwError $ ContractInstanceNotFound instanceId
          Just  c ->
            do
              caID <- maybe (throwError $ AesonDecodingError "Couldn't this JSON decode as `a`" (_contractInstanceId c))
                        pure
                        (decode . toLazyByteString . encodeUtf8Builder $ _contractInstanceContractId c)

              resp <- maybe (throwError $ ContractStateNotFound instanceId)
                        pure
                        (_contractInstanceState c >>= decodeText)

              let cd = getContract @a caID
              fromResponse @a instanceId cd resp

    join
      $ fmap extractState
      $ selectOne
      $ select
      $ do
          inst <- all_ (_contractInstances db)
          guard_ (inst ^. contractInstanceId ==. val_ (uuidStr instanceId))
          pure inst

  PutStopInstance instanceId ->
    updateRow
      $ update (_contractInstances db)
          (\ci -> ci ^. contractInstanceActive <-. val_ False)
          (\ci -> ci ^. contractInstanceId ==. val_ (uuidStr instanceId))

  GetActiveContracts ->
    fmap mkContracts
      $ selectList
      $ select
      $ do
          ci <- all_ (_contractInstances db)
          guard_ ( ci ^. contractInstanceActive )
          pure ci
