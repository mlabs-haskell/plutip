module Plutip.Config (
  PlutipConfig (..),
  WorkingDirectory (..),
  ExtraConfig (..),
  NominalDiffTime,
  EpochSize (..),
) where

import Cardano.Ledger.Slot (EpochSize (EpochSize, unEpochSize))
import Data.Default (Default, def)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Plutip.Launch.Extra.Types (ExtraConfig (ExtraConfig, ecEpochSize, ecMaxTxSize, ecRaiseExUnitsToMax, ecSlotLength))

-- | Configuration for the cluster working directory
-- This determines where the node database files will be stored for a running cluster
--
-- @since 0.2
data WorkingDirectory
  = -- | Stored in a temporary directory, deleted on cluster shutdown
    Temporary
  | -- | Stored in a specified directory
    Fixed
      { -- | Path for storing a cluster data, can be relative or absolute
        path :: FilePath
      , -- | Determines whether the working data should be kept on disk after cluster shutdown.
        --   Entire directory will be deleted on shutdown if False
        shouldKeep :: Bool
      }
  deriving stock (Generic, Show)

-- | Plutip configurable options
--
-- @since 0.2
data PlutipConfig = PlutipConfig
  { -- | Specify where genesis files, node config and etc are stored.
    --   In case of `Nothing` the `cluster-data` folder from the project's `data-files` is used.
    clusterDataDir :: Maybe FilePath
  , -- | Overrides a location of the cluster working directory,
    --   includes an option to keep the folder after cluster shutdown (`shouldKeep`).
    clusterWorkingDir :: WorkingDirectory
  , -- | Extra config to set slot length, epoch size, max tx size and max ex units
    --   for a local network.
    extraConfig :: ExtraConfig
  }
  deriving stock (Generic, Show)

instance Default PlutipConfig where
  def = PlutipConfig Nothing Temporary def
