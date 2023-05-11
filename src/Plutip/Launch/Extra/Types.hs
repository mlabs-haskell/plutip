module Plutip.Launch.Extra.Types (
  ExtraConfig (..),
  ExBudget (..),
  stdBlockExUnits,
  stdTxExUnits,
  stdTxSize,
  maxExUnits,
  stdCollateral,
  calculateCollateral,
) where

import Cardano.Ledger.Slot (EpochSize)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Ratio ((%))
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))

-- | Extra configuration options for setting slot length, epoch size, max tx size and
--   max ex units.
-- `ExtraConfig` is used both in `PlutipConfig` and `LocalClusterConfig` to pass
-- settings to the local cluster framework from the `cardano-wallet`.
-- `LocalClusterConfig` is defined in the `Plutip.Launch.Cluster` module, which
-- is an edited copy of the corresponing module from `cardano-wallet`.
-- `ExtraConfig` is used to keep custom changes closer together in order to
-- make diffs between Plutip's and `cardano-wallet` `Cluster.hs`'s smaller for
-- easier maintenance when updating `cardano-wallet`.
data ExtraConfig = ExtraConfig
  { ecSlotLength :: NominalDiffTime
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

maxExUnits :: ExBudget
maxExUnits = ExBudget (ExMemory maxBound) (ExCPU maxBound)

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
