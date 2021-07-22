{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent                       (threadDelay)
import           Control.Monad                            (forever)
import           Options.Applicative

import           Cardano.Api                              (Block (..), BlockHeader (..), BlockInMode (..), CardanoMode)
import           Cardano.Protocol.Socket.Client           (runChainSync)
import           Cardano.Slotting.Slot                    (SlotNo (..))
import           Ledger                                   (Slot)
import           Ledger.TimeSlot                          (SlotConfig (..))
import           Ouroboros.Consensus.Byron.Ledger.Block   (ByronHash (..))
import           Ouroboros.Consensus.Cardano.Block        (CardanoBlock, HardForkBlock (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import           Ouroboros.Consensus.Shelley.Protocol     (StandardCrypto)
import           Ouroboros.Network.Block                  (blockHash, blockSlot)

-- | We only need to know the location of the socket.
--   We can get the protocol versions from Cardano.Protocol.Socket.Type
data Configuration = Configuration
  { cSocketPath :: String
  } deriving (Show)

-- | This is the slot configuration for the shelley network, taken from the PAB
-- configuration file.
slotConfig :: SlotConfig
slotConfig =
  SlotConfig
    { scSlotZeroTime = 1591566291000
    , scSlotLength   = 1000
    }

-- | A simple callback that reads the incoming data, from the node.
processBlock
  :: BlockInMode CardanoMode
  -> Slot
  -> IO ()
processBlock (BlockInMode (Block (BlockHeader (SlotNo slot) hsh _) _) _) _ =
  putStrLn $ "Received block " <> show hsh
          <> " for slot "      <> show slot

getBlockHash
  :: CardanoBlock StandardCrypto
  -> String
getBlockHash = \case
  BlockByron blk ->
    show $ unByronHash $ blockHash blk
  BlockShelley (ShelleyBlock _ headerHash) ->
    show headerHash
  BlockAllegra (ShelleyBlock _ headerHash) ->
    show headerHash
  BlockMary (ShelleyBlock _ headerHash) ->
    show headerHash
  BlockAlonzo (ShelleyBlock _ headerHash) ->
    show headerHash

getBlockSlotNo
  :: CardanoBlock StandardCrypto
  -> String
getBlockSlotNo = \case
  BlockByron blk ->
    show $ unSlotNo $ blockSlot blk
  BlockShelley (ShelleyBlock _ headerHash) ->
    show headerHash
  BlockAllegra (ShelleyBlock _ headerHash) ->
    show headerHash
  BlockMary (ShelleyBlock _ headerHash) ->
    show headerHash
  BlockAlonzo (ShelleyBlock _ headerHash) ->
    show headerHash

cfgParser :: Parser Configuration
cfgParser = Configuration
  <$> argument str (metavar "SOCKET")

main :: IO ()
main = do
  cfg <- execParser $ info (cfgParser <**> helper)
                        ( fullDesc
                        <> progDesc "Connect and stream blocks from the node client"
                        )
  putStrLn "Runtime configuration:"
  putStrLn "----------------------"
  print    cfg
  _ <- runChainSync (cSocketPath cfg) slotConfig processBlock
  _ <- forever $ threadDelay 1000000
  pure ()
