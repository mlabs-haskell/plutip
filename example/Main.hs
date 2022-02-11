module Main (main) where

import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import DSL (
  ada,
  addSomeWallet,
  andThen,
  ledgerPaymentPkh,
  mkMainnetAddress,
  report,
  runContract,
  runContractTagged,
  runUsingCluster,
  waitSeconds,
 )
import Data.Text (Text, unpack)
import DebugContract.GetUtxos qualified as DebugContract
import DebugContract.LockUnlock qualified as FailBudget
import DebugContract.LockUnlockValidationFail qualified as FailValidation
import DebugContract.PayToWallet qualified as DebugContract
import LocalCluster.Types (supportDir)
import System.Environment (setEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Tools.Address qualified as Tools
import Tools.DebugCli qualified as CLI

main :: IO ()
main = do
  -- mapM_ (`hSetBuffering` NoBuffering) [stdout, stderr]
  -- todo: maybe some better configuring procedure should be introduced
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  -- setEnv "NO_CLEANUP" "0"

  runUsingCluster $ do
    w1 <- -- ? maybe it will be more ergonomic to get rid of `Ether` and just fail hard
    -- as there is no reason to continue if wallet can't be set up
      addSomeWallet (ada 101)
    w2 <- addSomeWallet (ada 202)
    waitSeconds 2

    -- debugWallets (sequence [w1, w2]) --temporary, for debugging
    testW1 <- either (error . show) pure w1
    testW2 <- either (error . show) pure w2

    -- 1 successful and 2 failing scenarios
    runContract testW1 DebugContract.getUtxos
      `andThen` report
    runContractTagged "Throws Contract error" testW1 DebugContract.getUtxosThrowsErr
      `andThen` report
    runContractTagged "Throws Exception" testW1 DebugContract.getUtxosThrowsEx
      `andThen` report

    -- successful wallet to wallet transaction
    let p2pContract = DebugContract.payTo (ledgerPaymentPkh testW2) 10_000_000
    runContractTagged "Pay wallet-to-wallet" testW1 p2pContract
      `andThen` report

    -- budget overspend script
    waitSeconds 2
    runContractTagged "Lock at script - budget overspend" testW1 FailBudget.lockAtScript
      `andThen` report
    waitSeconds 2
    runContractTagged "Spend from script - budget overspend" testW1 FailBudget.unlockFromScript
      `andThen` report

    -- validation fail script
    waitSeconds 2
    runContractTagged "Lock at script - validation fail" testW1 FailValidation.lockAtScript
      `andThen` report
    waitSeconds 2
    runContractTagged "Spend from script - validation fail" testW1 FailValidation.unlockFromScript
      `andThen` report

    liftIO $ putStrLn "Done. Debug awaiting - Enter to exit" >> void getLine
  where
    getAddr = either (error . show) unpack . Tools.ledgerToCardanoMainnet'
    debugWallets ws = do
      cEnv <- ask
      liftIO $ putStrLn "\nDebug check:"
      liftIO $ putStrLn $ "Cluster dir: " <> show (supportDir cEnv)
      either
        (error . ("Err: " <>) . show)
        (mapM_ (liftIO . CLI.utxoAtAddress cEnv . mkMainnetAddress))
        ws
