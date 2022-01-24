module Plutip (run) where

import LocalCluster.CardanoApi qualified as LCAPI
import LocalCluster.Cluster qualified as LC
import System.Environment (setEnv)
import Prelude

run :: IO ()
run = do
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  LC.runUsingCluster $ \runningNode -> do
    LCAPI.currentBlock runningNode >>= print