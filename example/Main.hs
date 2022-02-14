module Main (main) where

import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import DSL (
  ada,
  addSomeWallet,
  ledgerPaymentPkh,
  mkMainnetAddress,
  report,
  runContract,
  runUsingCluster,
  waitSeconds,
 )
import Data.Text (Text, unpack)
import DebugContract.GetUtxos qualified as DebugContract
import DebugContract.PayToWallet qualified as DebugContract
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
    testW1 <- either (error . show) pure w1
    runContract testW1 DebugContract.getUtxos
      >>= report
    runContract testW1 DebugContract.getUtxosThrowsErr
      >>= report
    runContract testW1 DebugContract.getUtxosThrowsEx
      >>= report

    testW2 <- either (error . show) pure w2
    runContract testW1 (DebugContract.payTo (ledgerPaymentPkh testW2) 10_000_000)
      >>= report
      >> debugWallets (sequence [w1, w2])

    liftIO $ putStrLn "Done. Debug awaiting - Enter to exit" >> void getLine
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
