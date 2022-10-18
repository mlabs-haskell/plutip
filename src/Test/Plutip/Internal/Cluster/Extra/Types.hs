module Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (..),
) where

import Cardano.Ledger.Slot (EpochSize)
import Data.Default (Default (def))
import Data.Time (NominalDiffTime)

data ExtraConfig = ExtraConfig
  { ecSlotLength :: NominalDiffTime
  , ecEpochSize :: EpochSize
  }
  deriving stock (Show)

instance Default ExtraConfig where
  def = ExtraConfig 0.2 160
