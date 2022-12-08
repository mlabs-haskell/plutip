module Test.Plutip.Config (
  PlutipConfig (..),
  WorkingDirectory (..),
  ChainIndexMode (..),
) where

import Cardano.Api (PaymentKey, SigningKey)
import Data.Default (Default, def)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig)

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
  , -- | in case of `Just path` relay node log will be saved to specified file
    relayNodeLogs :: Maybe FilePath
  , -- | the way of how `chain-index` is launched (default port, custom port, not launched),
    --   default mode - default port
    chainIndexMode :: ChainIndexMode
  , -- | Multiplier on all BPI transaction budgets
    budgetMultiplier :: Rational
  , -- | cluster file location override, when provided, includes a `shouldKeep`
    clusterWorkingDir :: WorkingDirectory
  , -- | Any extra pre-determined signers to use.
    --    Either provided by a path to the signing key file, or by the signing key itself.
    extraSigners :: [Either FilePath (SigningKey PaymentKey)]
  , -- | Extra config to set (at the moment) slot lenght and epoch size
    --   for local network
    extraConfig :: ExtraConfig
  }
  deriving stock (Generic, Show)

-- | The way to launch `chain-index`.
-- It is possible to not launch it at all.
data ChainIndexMode
  = -- | launch on default port `9083`
    DefaultPort
  | -- | launch on custom port
    CustomPort Natural
  | -- | do not launch at all
    NotNeeded
  deriving stock (Generic, Eq, Show)

instance Default PlutipConfig where
  def = PlutipConfig Nothing Nothing DefaultPort 1 Temporary [] def
