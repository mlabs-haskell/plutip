module Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (..),
  MaxExUnits (..),
  standardBlockExUnits,
  standardTxExUnits,
  increaseExUnits,
) where

import Cardano.Ledger.Slot (EpochSize)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

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
  , ecMaxTxSize :: Natural
  , ecIncreasedExUnits :: Natural
  }
  deriving stock (Show)

data MaxExUnits = MaxExUnits
  { exUnitsMem :: Natural
  , exUnitsSteps :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

standardTxExUnits :: MaxExUnits
standardTxExUnits =
  MaxExUnits
    { exUnitsMem = 10000000
    , exUnitsSteps = 10000000000
    }

standardBlockExUnits :: MaxExUnits
standardBlockExUnits =
  MaxExUnits
    { exUnitsMem = 50000000
    , exUnitsSteps = 40000000000
    }

increaseExUnits :: MaxExUnits -> Natural -> MaxExUnits
increaseExUnits exUnits factor =
  MaxExUnits
    { exUnitsMem = factor * (exUnitsMem exUnits)
    , exUnitsSteps = factor * (exUnitsSteps exUnits)
    }

instance Default ExtraConfig where
  def = ExtraConfig 0.1 80 16384 1
