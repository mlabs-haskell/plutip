module Main (main) where

import Control.Monad (forever)
import Data.Text (Text)
import LocalCluster.CardanoApi qualified as LCAPI
import LocalCluster.Cluster (runUsingCluster)
import LocalCluster.DebugCli qualified as CLI
import LocalCluster.Wallet
import System.Environment (setEnv)
import Utils (ada, waitSeconds)

main :: IO ()
main = do
  -- todo: maybe some better configuring procedure should be introduced
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  setEnv "CARDANO_NODE_TRACING_MIN_SEVERITY" "Debug"

  runUsingCluster $ \cEnv -> do
    LCAPI.currentBlock cEnv >>= print
    wallets <- -- adding several wallets
      addWallets
        cEnv
        [ mnemonicWallet testMnemonic (ada 11)
        , someWallet (ada 700)
        , someWallet (ada 42)
        ]
    singleWallet <- addWallet cEnv $ someWallet (ada 707) -- adding single wallet
    debugCheck cEnv (singleWallet : wallets)

    putStrLn "Done. Debug awaiting - interrupt to exit" >> forever (waitSeconds 60)
  where
    debugCheck cEnv ws = do
      putStrLn "\nDebug address check:"
      waitSeconds 2
      mapM_
        (CLI.utxoAtAddress cEnv . mainnetStringAddress)
        ws

testMnemonic :: [Text]
testMnemonic =
  [ "radar"
  , "scare"
  , "sense"
  , "winner"
  , "little"
  , "jeans"
  , "blue"
  , "spell"
  , "mystery"
  , "sketch"
  , "omit"
  , "time"
  , "tiger"
  , "leave"
  , "load"
  ]
