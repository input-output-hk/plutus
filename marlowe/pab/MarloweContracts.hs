{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module MarloweContracts(MarloweContracts(..), handlers) where

import           Control.Monad.Freer                 (interpret)
import           Data.Aeson                          (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Aeson.Types                    (prependFailure)
import           Data.Default                        (Default (def))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import qualified Language.Marlowe.Client             as Marlowe
import           Language.PureScript.Bridge          (equal, genericShow, mkSumType)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), HasDefinitions (..),
                                                      SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (psTypes))
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Text.Read                           (readMaybe)

data MarloweContracts =
    MarloweApp -- The main marlowe contract
    | WalletCompanion -- Wallet companion contract
    | MarloweFollower -- Follower contrat
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON MarloweContracts where
    toJSON k = object ["tag" .= show k]

instance FromJSON MarloweContracts where
    parseJSON = withObject "MarloweContracts" $ \m -> do
        (tg :: String) <- m .: "tag"
        case readMaybe tg of
            Just tg' -> pure tg'
            _        -> prependFailure "parsing MarloweContracts failed, " (fail $ "unexpected tag " <> tg)

instance Pretty MarloweContracts where
    pretty = viaShow

instance HasDefinitions MarloweContracts where
    getDefinitions = [ MarloweApp
                     , WalletCompanion
                     , MarloweFollower
                     ]
    getSchema = const [] -- TODO: replace with proper schemas using Builtin.endpointsToSchemas (missing some instances currently)
    getContract = \case
        MarloweApp      -> SomeBuiltin Marlowe.marlowePlutusContract
        WalletCompanion -> SomeBuiltin Marlowe.marloweCompanionContract
        MarloweFollower -> SomeBuiltin Marlowe.marloweFollowContract

instance HasPSTypes MarloweContracts where
    psTypes p = [ (equal <*> (genericShow <*> mkSumType)) p ]

handlers :: SimulatorEffectHandlers (Builtin MarloweContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin MarloweContracts) def
    $ interpret (contractHandler Builtin.handleBuiltin)
