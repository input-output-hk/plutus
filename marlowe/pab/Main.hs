{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Main(main, marloweTest) where

import           Control.Monad                       (guard, void)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson                          as JSON
import           Data.Aeson.Types                    (prependFailure)
import qualified Data.Aeson.Types                    as JSON
import           Data.Default                        (Default (def))
import qualified Data.Map                            as Map
import           Data.Maybe                          (listToMaybe)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import qualified Language.Marlowe.Client             as Marlowe
import           Language.Marlowe.Semantics          (Action (..), Case (..), Contract (..), MarloweParams, Party (..),
                                                      Payee (..), Value (..))
import qualified Language.Marlowe.Semantics          as Marlowe
import           Language.Marlowe.Util               (ada)
import           Ledger                              (PubKeyHash, Slot, pubKeyHash)
import qualified Ledger.Value                        as Val
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified PlutusTx.AssocMap                   as AssocMap
import           Text.Read                           (readMaybe)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin Marlowe) "Starting marlowe PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    void $ liftIO getLine
    shutdown

marloweTest :: IO ()
marloweTest = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin Marlowe) "Starting marlowe PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    (newWallet, newPubKey) <- Simulator.addWallet @(Builtin Marlowe)
    Simulator.logString @(Builtin Marlowe) "Created new wallet"
    walletCompanionId <- Simulator.activateContract newWallet WalletCompanion
    Simulator.logString @(Builtin Marlowe) "Activated companion contract"
    marloweContractId <- Simulator.activateContract newWallet MarloweApp
    Simulator.logString @(Builtin Marlowe) "Activated marlowe contract"

    void $ Simulator.waitNSlots 10

    let args = let h = (pubKeyHash newPubKey) in createArgs h h
    void $ Simulator.callEndpointOnInstance marloweContractId "create" args

    followerId <- Simulator.activateContract newWallet MarloweFollower
    Simulator.logString @(Builtin Marlowe) "Activated marlowe follower"

    mp <- Simulator.waitForState @(Builtin Marlowe) extractMarloweParams walletCompanionId
    Simulator.logString @(Builtin Marlowe) $ "Found marlowe params: " <> show mp

    _ <- Simulator.waitForEndpoint followerId "follow"
    Simulator.logString @(Builtin Marlowe) $ "Calling endpoint on marlowe follow"
    _ <- Simulator.callEndpointOnInstance followerId "follow" mp

    followState <- Simulator.waitForState @(Builtin Marlowe) extractFollowState followerId

    Simulator.logString @(Builtin Marlowe) $ "Follow state: " <> show followState

    shutdown

extractMarloweParams :: JSON.Value -> Maybe MarloweParams
extractMarloweParams vl = do
    (Marlowe.CompanionState s) <- either (const Nothing) Just (JSON.parseEither JSON.parseJSON vl)
    (params, _) <- listToMaybe $ Map.toList s
    pure params

extractFollowState :: JSON.Value -> Maybe Marlowe.ContractHistory
extractFollowState vl = do
    s <- either (const Nothing) Just (JSON.parseEither JSON.parseJSON vl)
    guard (not $ Marlowe.isEmpty s)
    pure s

createArgs :: PubKeyHash -> PubKeyHash -> (AssocMap.Map Val.TokenName PubKeyHash, Marlowe.Contract)
createArgs investor issuer = (tokenNames, zcb) where
    tokenNames = AssocMap.fromList [("Investor", investor), ("Issuer", issuer)]
    issuerAcc = Role "Issuer"
    investorAcc = Role "Investor"
    zcb = When
            [ Case
                (Deposit issuerAcc issuerAcc ada (Constant 850))
                (Pay issuerAcc (Account investorAcc) ada (Constant 850)
                    (When
                        [ Case (Deposit issuerAcc investorAcc ada (Constant 1000)) Close
                        ] (26936589 :: Slot) Close
                    )
                )
            ]
            (26936589 :: Slot) Close

data Marlowe =
    MarloweApp -- the main marlowe contract
    | WalletCompanion -- wallet companion contract
    | MarloweFollower -- follower contrat
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Marlowe where
    toJSON k = object ["tag" .= show k]

instance FromJSON Marlowe where
    parseJSON = withObject "Marlowe" $ \m -> do
        (tg :: String) <- m .: "tag"
        case readMaybe tg of
            Just tg' -> pure tg'
            _        -> prependFailure "parsing Marlowe failed, " (fail $ "unexpected tag " <> tg)

instance Pretty Marlowe where
    pretty = viaShow
handleMarloweContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin Marlowe))) effs
    )
    => ContractEffect (Builtin Marlowe)
    ~> Eff effs
handleMarloweContract = Builtin.handleBuiltin getSchema getContract where
    getSchema = const [] -- TODO: replace with proper schemas using Builtin.endpointsToSchemas (missing some instances currently)
    getContract = \case
        MarloweApp      -> SomeBuiltin Marlowe.marlowePlutusContract
        WalletCompanion -> SomeBuiltin Marlowe.marloweCompanionContract
        MarloweFollower -> SomeBuiltin Marlowe.marloweFollowContract

handlers :: SimulatorEffectHandlers (Builtin Marlowe)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin Marlowe) def def [MarloweApp]
    $ interpret handleMarloweContract
