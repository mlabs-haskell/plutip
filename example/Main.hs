module Main (main) where

import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text, unpack)
import DebugContract.GetUtxos qualified as GetUtxos
import DebugContract.LockUnlock qualified as LockUnlock
import DebugContract.LockUnlockValidationFail qualified as LockUnlockValidationFail
import DebugContract.PayToWallet qualified as PayToWallet
import System.Environment (setEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Test.Plutip (
  ada,
  addSomeWallet,
  andThen,
  ledgerPaymentPkh,
  mkMainnetAddress,
  report,
  runContractWithReport,
  runUsingCluster,
  waitSeconds,
 )

import Data.Default (def)
import Test.Plutip.Internal.LocalCluster.Cluster (runUsingClusterConf)
import Test.Plutip.Internal.LocalCluster.Config (relayNodeLogs)

main :: IO ()
main = do
  let conf = def {relayNodeLogs = Just "example/relay-node.log"}
  runUsingClusterConf conf $ do
    testW1 <- addSomeWallet (ada 101)
    testW2 <- addSomeWallet (ada 202)
    waitSeconds 2 -- wait for transactions to submit

    -- 1 successful and 2 failing scenarios
    runContractWithReport "Get utxos" testW1 GetUtxos.getUtxos
    runContractWithReport "Throws Contract error" testW1 GetUtxos.getUtxosThrowsErr
    runContractWithReport "Throws Exception" testW1 GetUtxos.getUtxosThrowsEx

    -- successful wallet to wallet transaction
    let p2pContract = PayToWallet.payTo (ledgerPaymentPkh testW2) 10_000_000
    runContractWithReport "Pay wallet-to-wallet" testW1 p2pContract

    -- budget overspend script
    runContractWithReport
      "Lock at script then spend - budget overspend"
      testW1
      LockUnlock.lockThenSpend

    -- validation fail script
    runContractWithReport
      "Lock at script then spend - validation fail"
      testW1
      LockUnlockValidationFail.lockThenSpend

    liftIO $ putStrLn "Done. Debug awaiting - Enter to exit" >> void getLine
