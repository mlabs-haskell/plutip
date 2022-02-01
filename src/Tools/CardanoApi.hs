module Tools.CardanoApi
  ( currentBlock,
    utxosAtAddress,
  queryProtocolParams)
where

import Cardano.Api qualified as C
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (..))
import Control.Exception (Exception)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import LocalCluster.Types
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

data CardanoApiError
  = SomeError String
  deriving stock (Eq, Show, Generic)

instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
currentBlock :: ClusterEnv -> IO (Either AcquireFailure (WithOrigin C.BlockNo))
currentBlock (ClusterEnv rn _ _) = do
  let query = C.QueryChainBlockNo
      info = debugConnectionInfo rn
  C.queryNodeLocalState info Nothing query

utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.AlonzoEra))
utxosAtAddress (ClusterEnv rn _ _) addr = do
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = debugConnectionInfo rn
    query =
      shellyBasedAlonzoQuery
        (C.QueryUTxO $ C.QueryUTxOByAddress (Set.singleton addr))

queryProtocolParams :: ClusterEnv -> IO (Either CardanoApiError ProtocolParameters)
queryProtocolParams (ClusterEnv rn _ _) =
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = debugConnectionInfo rn
    query = shellyBasedAlonzoQuery C.QueryProtocolParameters

debugConnectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
debugConnectionInfo (RunningNode socket _ _) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    C.Mainnet
    -- C.Testnet $ C.NetworkMagic 8
    (nodeSocketFile socket)

shellyBasedAlonzoQuery 
  :: C.QueryInShelleyBasedEra C.AlonzoEra result1
  -> C.QueryInMode C.CardanoMode (Either EraMismatch result1)
shellyBasedAlonzoQuery =
  C.QueryInEra C.AlonzoEraInCardanoMode
    . C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo

flattenQueryResult 
  :: (Show e1, Show e2, Show b)
  => Either e1 (Either e2 b) 
  -> Either CardanoApiError b
flattenQueryResult = \case
  Right (Right res) -> Right res
  err -> Left $ SomeError (show err)
