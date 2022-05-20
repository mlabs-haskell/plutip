module Test.Plutip.Config (
  PlutipConfig (..),
) where

import Data.Default (Default, def)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Cardano.Api (PaymentKey, SigningKey)

-- | Plutip configurable options
--
-- @since 0.2
data PlutipConfig = PlutipConfig
  { -- | in case of `Nothing` cluster data from project `data-files` is used
    clusterDataDir :: Maybe FilePath
  , -- | in case of `Just path` relay node log will be saved to specified file
    relayNodeLogs :: Maybe FilePath
  , -- | in case of `Nothing` port from `Plutus.ChainIndex.Config.defaultConfig` is used
    chainIndexPort :: Maybe Natural
  , -- | Any extra pre-determined signers to use.
    --    Either provided by a path to the signing key file, or by the signing key itself.
    extraSigners :: [Either FilePath (SigningKey PaymentKey)]
  }
  deriving stock (Generic)

instance Default PlutipConfig where
  def = PlutipConfig Nothing Nothing Nothing []
