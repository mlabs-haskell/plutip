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
import Plutip.Launch.Extra.Types (ExtraConfig (ExtraConfig, ecEpochSize, ecSlotLength))

-- | Configuration for the cluster working directory
-- This determines where the node database, chain-index database,
-- and bot-plutus-interface files will be stored for a running cluster
--
-- @since 0.2
data WorkingDirectory
  = -- | Stored in a temporary directory, deleted on cluster shutdown
    Temporary
  | -- | Stored in a set directory
    Fixed
      { -- | Path to store cluster data, can be relative or absolute
        path :: FilePath
      , -- | Should the working data be kept on disk after cluster shutdown.
        --   Full directory will be deleted on shutdown if False
        shouldKeep :: Bool
      }
  deriving stock (Generic, Show)

-- | Plutip configurable options
--
-- @since 0.2
data PlutipConfig = PlutipConfig
  { -- | in case of `Nothing` cluster data from project `data-files` is used
    clusterDataDir :: Maybe FilePath
  , -- | cluster file location override, when provided, includes a `shouldKeep`
    clusterWorkingDir :: WorkingDirectory
  , -- | Extra config to set (at the moment) slot lenght and epoch size
    --   for local network
    extraConfig :: ExtraConfig
  }
  deriving stock (Generic, Show)

instance Default PlutipConfig where
  def = PlutipConfig Nothing Temporary def
