{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main(main) where

import qualified Cardano.Api                    as C
import qualified Cardano.Api.Shelley            as C
import qualified Cardano.Binary                 as Binary
import qualified Control.Foldl                  as L
import           Control.Monad.Freer            (run)
import qualified Data.Aeson                     as Aeson
import           Data.Aeson.Encode.Pretty       (encodePretty)
import           Data.Bitraversable             (bitraverse)
import qualified Data.ByteString.Lazy           as BSL
import           Data.Default                   (Default (..))
import           Data.Foldable                  (traverse_)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Proxy                     (Proxy (..))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text.Prettyprint.Doc      (Pretty (..))
import           Data.Typeable                  (Typeable)
import           Flat                           (flat)
import           GHC.Generics                   (Generic)
import qualified Ledger                         as Plutus
import           Ledger.Constraints.OffChain    (UnbalancedTx (..))
import           Ledger.Index                   (ScriptValidationEvent (..))
import           Options.Applicative
import qualified Plutus.Contract.CardanoAPI     as CardanoAPI
import qualified Plutus.Contracts.Crowdfunding  as Crowdfunding
import qualified Plutus.Contracts.Uniswap.Trace as Uniswap
import           Plutus.Trace.Emulator          (EmulatorConfig, EmulatorTrace)
import qualified Plutus.Trace.Emulator          as Trace
import           Plutus.V1.Ledger.Api           (ExBudget (..))
import           Plutus.V1.Ledger.Scripts       (Script (..))
import qualified Spec.Auction                   as Auction
import qualified Spec.Currency                  as Currency
import qualified Spec.Escrow                    as Escrow
import qualified Spec.Future                    as Future
import qualified Spec.GameStateMachine          as GameStateMachine
import qualified Spec.MultiSig                  as MultiSig
import qualified Spec.MultiSigStateMachine      as MultiSigStateMachine
import qualified Spec.PingPong                  as PingPong
import qualified Spec.Prism                     as Prism
import qualified Spec.PubKey                    as PubKey
import qualified Spec.Stablecoin                as Stablecoin
import qualified Spec.TokenAccount              as TokenAccount
import qualified Spec.Vesting                   as Vesting
import qualified Streaming.Prelude              as S
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                ((</>))
import qualified Wallet.Emulator.Folds          as Folds
import           Wallet.Emulator.Stream         (foldEmulatorStreamM)

data Command =
    Scripts
    | Transactions{ networkId :: C.NetworkId, protocolParamsJSON :: FilePath }
    deriving stock (Show, Eq)

writeWhat :: Command -> String
writeWhat Scripts        = "scripts"
writeWhat Transactions{} = "transactions"

pathParser :: Parser FilePath
pathParser = strArgument (metavar "SCRIPT_PATH" <> help "output path")

protocolParamsParser :: Parser FilePath
protocolParamsParser = strOption (long "protocol-parameters" <> short 'p' <> help "Path to protocol parameters JSON file" <> showDefault <> value "protocol-parameters.json")

networkIdParser :: Parser C.NetworkId
networkIdParser =
    let p = C.Testnet . C.NetworkMagic <$> option auto (long "network-magic" <> short 'n' <> help "Cardano network magic. If none is specified, mainnet addresses are generated.")
    in p <|> pure C.Mainnet

commandParser :: Parser Command
commandParser = subparser $ mconcat [scriptsParser, transactionsParser]

scriptsParser :: Mod CommandFields Command
scriptsParser =
    command "scripts" $
    info
        (pure Scripts)
        (fullDesc <> progDesc "Write fully applied validator scripts")

transactionsParser :: Mod CommandFields Command
transactionsParser =
    command "transactions" $
    info
        (Transactions <$> networkIdParser <*> protocolParamsParser)
        (fullDesc <> progDesc "Write partial transactions")

data ScriptsConfig =
    ScriptsConfig
        { scPath    :: FilePath
        , scCommand :: Command
        }

progParser :: ParserInfo ScriptsConfig
progParser =
    let p = ScriptsConfig <$> pathParser <*> commandParser
    in info
        (p <**> helper)
        (fullDesc
        <> progDesc "Run a number of emulator traces and write all validator scripts and/or partial transactions to SCRIPT_PATH"
        <> header "plutus-use-cases-scripts - extract applied validators and partial transactions from emulator traces"
        )

main :: IO ()
main = execParser progParser >>= writeScripts

writeScripts :: ScriptsConfig -> IO ()
writeScripts config = do
    putStrLn $ "Writing " <> writeWhat (scCommand config) <> " to: " <> scPath config
    traverse_ (uncurry3 (writeScriptsTo config))
        [ ("auction_1", Auction.auctionTrace1, Auction.auctionEmulatorCfg)
        , ("auction_2", Auction.auctionTrace2, Auction.auctionEmulatorCfg)
        , ("crowdfunding-success", Crowdfunding.successfulCampaign, def)
        , ("currency", Currency.currencyTrace, def)
        , ("escrow-redeem_1", Escrow.redeemTrace, def)
        , ("escrow-redeem_2", Escrow.redeem2Trace, def)
        , ("escrow-refund", Escrow.refundTrace, def)
        , ("future-increase-margin", Future.increaseMarginTrace, def)
        , ("future-settle-early", Future.settleEarlyTrace, def)
        , ("future-pay-out", Future.payOutTrace, def)
        , ("game-sm-success", GameStateMachine.successTrace, def)
        , ("game-sm-success_2", GameStateMachine.successTrace2, def)
        , ("multisig-success", MultiSig.succeedingTrace, def)
        , ("multisig-failure", MultiSig.failingTrace, def)
        , ("multisig-sm", MultiSigStateMachine.lockProposeSignPay 3 2, def)
        , ("ping-pong", PingPong.pingPongTrace, def)
        , ("ping-pong_2", PingPong.twoPartiesTrace, def)
        , ("prism", Prism.prismTrace, def)
        , ("pubkey", PubKey.pubKeyTrace, def)
        , ("stablecoin_1", Stablecoin.stablecoinTrace, def)
        , ("stablecoin_2", Stablecoin.maxReservesExceededTrace, def)
        , ("token-account", TokenAccount.tokenAccountTrace, def)
        , ("vesting", Vesting.retrieveFundsTrace, def)
        , ("uniswap", Uniswap.uniswapTrace, def)
        ]

{-| Run an emulator trace and write the applied scripts to a file in Flat format
    using the name as a prefix.
-}
writeScriptsTo
    :: ScriptsConfig
    -> String
    -> EmulatorTrace a
    -> EmulatorConfig
    -> IO ()
writeScriptsTo ScriptsConfig{scPath, scCommand} prefix trace emulatorCfg = do
    let (scriptEvents, balanceEvents) =
            S.fst'
            $ run
            $ foldEmulatorStreamM (L.generalize theFold)
            $ Trace.runEmulatorStream emulatorCfg def trace

    createDirectoryIfMissing True scPath
    case scCommand of
        Scripts -> do
            traverse_ (uncurry $ writeScript scPath prefix) (zip [1::Int ..] scriptEvents)
        Transactions{networkId, protocolParamsJSON} -> do
            bs <- BSL.readFile protocolParamsJSON
            case Aeson.eitherDecode bs of
                Left err -> putStrLn err
                Right params -> traverse_ (uncurry $ writeTransaction params networkId scPath prefix) (zip [1::Int ..] balanceEvents)

{- There's an instance of Codec.Serialise for
    Script in Scripts.hs (see Note [Using Flat inside CBOR instance of Script]),
    which wraps Flat-encoded bytestings in CBOR, but that's not used here: we
    just use unwrapped Flat because that's more convenient for use with the
    `plc` command, for example.
-}
writeScript :: FilePath -> String -> Int -> ScriptValidationEvent -> IO ()
writeScript fp prefix idx ScriptValidationEvent{sveScript, sveResult} = do
    let filename = fp </> prefix <> "-" <> show idx <> ".flat"
    putStrLn $ "Writing script: " <> filename <> " (Cost: " <> either show (showBudget . fst) sveResult <> ")"
    BSL.writeFile filename (BSL.fromStrict . flat . unScript $ sveScript)
    where
        showBudget (ExBudget exCPU exMemory) = show exCPU <> ", " <> show exMemory

writeTransaction :: C.ProtocolParameters -> C.NetworkId -> FilePath -> String -> Int -> UnbalancedTx -> IO ()
writeTransaction params networkId fp prefix idx tx = do
    let filename1 = fp </> prefix <> "-" <> show idx <> ".json"
    putStrLn $ "Writing partial transaction JSON: " <> filename1
    BSL.writeFile filename1 (encodePretty tx)
    case export params networkId tx of
        Left err -> putStrLn $ "Export tx failed for " <> filename1 <> ". Reason: " <> show (pretty err)
        Right exportTx -> do
            let filename2 = fp </> prefix <> "-" <> show idx <> ".cbor"
            putStrLn $ "Writing partial transaction CBOR: " <> filename2
            BSL.writeFile filename2 $ BSL.fromStrict (Binary.serialize' exportTx)

-- | `uncurry3` converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

theFold :: Folds.EmulatorEventFold ([ScriptValidationEvent], [UnbalancedTx])
theFold = (,) <$> Folds.scriptEvents <*> Folds.walletTxBalanceEvents

-- | Partial transaction that can be balanced by the wallet backend.
data ExportTx =
        ExportTx
            { partialTx   :: C.Tx C.AlonzoEra -- ^ The transaction itself
            , lookups     :: [(C.TxIn, C.TxOut C.AlonzoEra)] -- ^ The tx outputs for all inputs spent by the partial tx
            , signatories :: [C.Hash C.PaymentKey] -- ^ Key(s) that we expect to be used for balancing & signing. (Advisory)
            }
    deriving stock (Generic, Typeable)

instance Binary.ToCBOR ExportTx where
    toCBOR ExportTx{partialTx, lookups, signatories} =
        Binary.toCBOR
            -- This is the best I could do, the types in Cardano.API all seem to have different serialisation
            -- formats (ToCBOR, SerialiseAsCBOR, ToJSON)
            ( C.serialiseToCBOR partialTx
            , Aeson.encode lookups -- TODO: Missing CBOR instance(s) for TxOut AlonzoEra :(
            , Binary.serialize' signatories
            )

    encodedSizeExpr size _ =
        Binary.encodedSizeExpr
            size
            (Proxy @(Binary.LengthOf BSL.ByteString, Binary.LengthOf BSL.ByteString, Binary.LengthOf BSL.ByteString))

instance C.HasTypeProxy ExportTx where
    data AsType ExportTx = AsExportTx
    proxyToAsType _ = AsExportTx

export :: C.ProtocolParameters -> C.NetworkId -> UnbalancedTx -> Either CardanoAPI.ToCardanoError ExportTx
export params networkId UnbalancedTx{unBalancedTxTx, unBalancedTxUtxoIndex, unBalancedTxRequiredSignatories} =
    ExportTx
        <$> mkTx params networkId unBalancedTxTx
        <*> mkLookups networkId unBalancedTxUtxoIndex
        <*> mkSignatories unBalancedTxRequiredSignatories

mkTx :: C.ProtocolParameters -> C.NetworkId -> Plutus.Tx -> Either CardanoAPI.ToCardanoError (C.Tx C.AlonzoEra)
mkTx params networkId = fmap (C.makeSignedTransaction []) . CardanoAPI.toCardanoTxBody (Just params) networkId

mkLookups :: C.NetworkId -> Map Plutus.TxOutRef Plutus.TxOut -> Either CardanoAPI.ToCardanoError [(C.TxIn, C.TxOut C.AlonzoEra)]
mkLookups networkId = traverse (bitraverse CardanoAPI.toCardanoTxIn (CardanoAPI.toCardanoTxOut networkId)) . Map.toList

mkSignatories :: Set Plutus.PubKeyHash -> Either CardanoAPI.ToCardanoError [C.Hash C.PaymentKey]
mkSignatories = traverse CardanoAPI.toCardanoPaymentKeyHash . Set.toList
