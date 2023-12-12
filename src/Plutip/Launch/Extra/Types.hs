module Plutip.Launch.Extra.Types (
  ExtraConfig (..),
  ExBudget (..),
  stdBlockExUnits,
  stdTxExUnits,
  stdTxSize,
  stdCollateral,
  calculateCollateral,
) where

import Cardano.Slotting.Slot (EpochSize)
import Cardano.Ledger.Shelley.Genesis (NominalDiffTimeMicro)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Ratio ((%))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))

-- | Extra configuration options to set slot length and epoch size for local network.
--   `ExtraConfig` used both in `PlutipConfig` and `LocalClusterConfig` to pass
-- settings from Plutip user to local cluster framework of `cardano-wallet`.
-- As `Cluster.hs` module, where `LocalClusterConfig` is defined, is copy of corresponding
-- module from `cardano-wallet` framework,
-- `ExtraConfig` is used to keep custom changes closer together to make diffs between copy
-- and original `Cluster.hs` module smaller for easier maintenance during updates.
data ExtraConfig = ExtraConfig
  { ecSlotLength :: NominalDiffTimeMicro
  , ecEpochSize :: EpochSize
  , ecMaxTxSize :: Natural
  , ecRaiseExUnitsToMax :: Bool
  }
  deriving stock (Show)

data ExBudget = ExBudget
  { exUnitsMem :: ExMemory
  , exUnitsSteps :: ExCPU
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- below from https://github.com/input-output-hk/cardano-node/blob/master/configuration/cardano/mainnet-alonzo-genesis.json
stdTxExUnits :: ExBudget
stdTxExUnits = ExBudget (ExMemory 10000000) (ExCPU 10000000000)

stdBlockExUnits :: ExBudget
stdBlockExUnits = ExBudget (ExMemory 50000000) (ExCPU 40000000000)

instance Bounded ExBudget where
  minBound = ExBudget (ExMemory minBound) (ExCPU minBound)
  maxBound = ExBudget (ExMemory maxBound) (ExCPU maxBound)

stdCollateral :: Natural
stdCollateral = 150

stdTxSize :: Natural
stdTxSize = 16384

-- | Necessary when increasing TxSize so as not raise collateral above expected.
calculateCollateral :: Natural -> Natural
calculateCollateral maxTxSize =
  if maxTxSize > stdTxSize
    then truncate $ stdCollateral * stdTxSize % maxTxSize
    else stdCollateral

instance Default ExtraConfig where
  def = ExtraConfig 0.1 80 stdTxSize False
