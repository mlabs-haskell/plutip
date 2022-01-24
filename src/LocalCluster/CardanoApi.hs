module LocalCluster.CardanoApi (currentBlock) where

import Cardano.Api qualified as C
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Prelude

currentBlock :: RunningNode -> IO (Either AcquireFailure (WithOrigin C.BlockNo))
currentBlock rn = do
  let query = C.QueryChainBlockNo
      info = debugConnectionInfo rn
  C.queryNodeLocalState info Nothing query

debugConnectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
debugConnectionInfo (RunningNode socket _ _) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    C.Mainnet
    -- C.Testnet $ C.NetworkMagic 8
    (nodeSocketFile socket)