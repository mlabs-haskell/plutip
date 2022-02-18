module Test.Plutip.Internal.LocalCluster.Config (
  Config (..),
) where

import Data.Default (Default, def)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

data Config = Config
  { clusterDataDir :: Maybe FilePath
  , relayNodeLogs :: Maybe FilePath
  , chainIndexPort :: Maybe Natural
  -- , slotLength :: TBD
  }
  deriving stock (Generic)

instance Default Config where
  def = Config Nothing Nothing Nothing
