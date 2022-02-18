module Test.Plutip.Options where

import Data.Default (def)
import Data.Tagged (Tagged (Tagged))
import Numeric.Natural (Natural)
import Test.Plutip.Internal.LocalCluster.Config (
  Config (Config, chainIndexPort, clusterDataDir, relayNodeLogs),
 )
import Test.Tasty.Options (
  IsOption,
  OptionSet,
  defaultValue,
  lookupOption,
  optionHelp,
  optionName,
  parseValue,
  showDefaultValue,
 )

-- TODO: not sure if we will be able to use it with tasty integration
-- but let it be for now, just in case

-- | Directory with data required for cluster launch
newtype DataDir = DataDir (Maybe FilePath)
  deriving stock (Show)

instance IsOption DataDir where
  defaultValue = DataDir (clusterDataDir def)
  parseValue = const Nothing
  optionName = Tagged "cluster-data-dir"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

-- | Optionally save relay node logs to desired location
data RelayLogs
  = NotRequired
  | SaveLogs FilePath
  deriving stock (Show)

instance IsOption RelayLogs where
  defaultValue = maybe NotRequired SaveLogs (relayNodeLogs def)
  parseValue = const Nothing
  optionName = Tagged "relay-node-log-dir"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

-- | Port for `chain-index` (to prevent potential conflicts)
data ChainIndexPort
  = Exact Natural
  | -- | could be unsafe
    SomeUnused
  deriving stock (Show)

instance IsOption ChainIndexPort where
  defaultValue = maybe SomeUnused Exact (chainIndexPort def)
  parseValue = const Nothing
  optionName = Tagged "chain-index-port"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

data SlotLength -- TBD

toClusterConfig :: OptionSet -> Config
toClusterConfig ops =
  let (DataDir dir) = lookupOption @DataDir ops
      logs = case lookupOption @RelayLogs ops of
        NotRequired -> Nothing
        SaveLogs file -> Just file
      cixPort = case lookupOption @ChainIndexPort ops of
        SomeUnused -> Nothing
        Exact port -> Just port
   in Config
        { clusterDataDir = dir
        , relayNodeLogs = logs
        , chainIndexPort = cixPort
        }
