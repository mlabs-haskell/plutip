module Main (main) where

import Prelude
import System.Environment (setEnv, getEnv)
import LocalCluster.CardanoApi qualified as LCAPI
import LocalCluster.Cluster (runUsingCluster)
import LocalCluster.DebugCli qualified as CLI
import LocalCluster.Wallet
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = do
  -- todo: maybe some better configuring procedure should be introduced
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"

  getEnv "SHELLEY_TEST_DATA" >>= putStrLn

  runUsingCluster $ \cEnv -> do
    LCAPI.currentBlock cEnv >>= print
    wallets <- -- adding several wallets
      addWallets cEnv
        [ mnemonicWallet testMnemonic (ada 11)
        , someWallet (ada 700)
        , someWallet (ada 42)
        ]
    singleWallet <- addWallet cEnv $ someWallet (ada 707) -- adding single wallet
    waitSeconds 2
    mapM_ 
      (CLI.utxoAtAddress cEnv . stringAddress) 
      (singleWallet : wallets)

    putStrLn "Interrupt to exit" >> forever (waitSeconds 60)

ada v = 1_000_000 * v

testMnemonic :: [Text]
testMnemonic =
  [ "radar",
    "scare",
    "sense",
    "winner",
    "little",
    "jeans",
    "blue",
    "spell",
    "mystery",
    "sketch",
    "omit",
    "time",
    "tiger",
    "leave",
    "load"
  ]

waitSeconds :: Int -> IO ()
waitSeconds x = threadDelay $ x * 1000000