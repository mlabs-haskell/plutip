module Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (..),
) where

import Cardano.Ledger.Slot (EpochSize)
import Data.Default (Default (def))
import Data.Time (NominalDiffTime)

-- | Extra configuration options to set slot length and epoch size for local network.
--   `ExtraConfig` used both in `PlutipConfig` and `LocalClusterConfig` to pass
-- settings from Plutip user to local cluster framework of `cardano-wallet`.
-- As `Cluster.hs` module, where `LocalClusterConfig` is defined, is copy of corresponding
-- module from `cardano-wallet` framework,
-- `ExtraConfig` is used to keep custom changes closer together to make diffs between copy
-- and original `Cluster.hs` module smaller for easier maintenance during updates.
data ExtraConfig = ExtraConfig
  { ecSlotLength :: NominalDiffTime
  , ecEpochSize :: EpochSize
  }
  deriving stock (Show)

instance Default ExtraConfig where
  def = ExtraConfig 0.2 160
