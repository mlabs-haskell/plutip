module LocalCluster.CardanoApi (
  currentBlock,
  utxosAtAddress,
) where

import Cardano.Api qualified as C
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Cardano.Wallet.Primitive.Types.Address (Address)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (..))
import Control.Exception (Exception)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import LocalCluster.Types
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import Address qualified

data CardanoApiError
  = OtherError String
  deriving stock (Eq, Show, Generic)

instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
currentBlock :: ClusterEnv -> IO (Either AcquireFailure (WithOrigin C.BlockNo))
currentBlock (ClusterEnv rn _ _) = do
  let query = C.QueryChainBlockNo
      info = debugConnectionInfo rn
  C.queryNodeLocalState info Nothing query

utxosAtAddress :: ClusterEnv -> Address -> IO (Either CardanoApiError (C.UTxO C.AlonzoEra))
utxosAtAddress (ClusterEnv rn _ _) addr =
  either
    (pure . Left . OtherError . show) -- fixme: some better errors structure
    queryAddress
    (Address.walletToCardanoAny addr)
  where
    info = debugConnectionInfo rn
    queryAddress addrAny = do
      res <- C.queryNodeLocalState info Nothing (mkQuery addrAny)
      pure $ case res of
        Right (Right utxos) -> Right utxos
        err -> Left $ OtherError (show err)

    mkQuery addr' =
      C.QueryInEra C.AlonzoEraInCardanoMode $
        C.QueryInShelleyBasedEra
          C.ShelleyBasedEraAlonzo
          (C.QueryUTxO $ C.QueryUTxOByAddress (Set.singleton addr'))

debugConnectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
debugConnectionInfo (RunningNode socket _ _) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    C.Mainnet
    -- C.Testnet $ C.NetworkMagic 8
    (nodeSocketFile socket)
