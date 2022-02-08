module Main (main) where

import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import DSL (
  ada,
  addSomeWallet,
  mkMainnetAddress,
  report,
  runContract,
  runUsingCluster,
  waitSeconds,
 )
import Data.Text (Text, unpack)
import DebugContract.DebugGet qualified as DebugContract
import LocalCluster.Types (supportDir)
import System.Environment (setEnv)
import Tools.DebugCli qualified as CLI

main :: IO ()
main = do
  -- todo: maybe some better configuring procedure should be introduced
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"

  runUsingCluster $ do
    w1 <- -- ? maybe it will be more ergonomic to get rid of `Ether` and just fail hard
    -- as there is no reason to continue if wallet can't be set up
      addSomeWallet (ada 101)
    w2 <- addSomeWallet (ada 202)

    debugWallets (sequence [w1, w2]) --temporary, for debugging
    testWallet <- either (error . show) pure w1
    runContract testWallet DebugContract.getUtxos
      >>= report
    runContract testWallet DebugContract.getUtxosThrowsErr
      >>= report
    runContract testWallet DebugContract.getUtxosThrowsEx
      >>= report

  putStrLn "Done. Debug awaiting - interrupt to exit" >> forever (waitSeconds 60)
  where
    debugWallets ws = do
      cEnv <- ask
      liftIO $ putStrLn "\nDebug check:"
      liftIO $ putStrLn $ "Cluster dir: " <> show (supportDir cEnv)
      liftIO $ waitSeconds 2
      either
        (error . ("Err: " <>) . show)
        (mapM_ (liftIO . CLI.utxoAtAddress cEnv . mkMainnetAddress))
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
