module Main (main) where

import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text, unpack)
import System.Environment (setEnv)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Test.Plutip (
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
import Test.Plutip.DebugContract.GetUtxos qualified as DebugContract
import Test.Plutip.DebugContract.LockUnlock qualified as FailBudget
import Test.Plutip.DebugContract.LockUnlockValidationFail qualified as FailValidation
import Test.Plutip.DebugContract.PayToWallet qualified as DebugContract
import Test.Plutip.LocalCluster.Types (supportDir)

main :: IO ()
main = do
  runUsingCluster $ do
    testW1 <- addSomeWallet (ada 101)
    testW2 <- addSomeWallet (ada 202)
    waitSeconds 2 -- wait for transactions to submit

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
    runContractTagged
      "Lock at script then spend - budget overspend"
      testW1
      FailBudget.lockThenSpend
      `andThen` report

    -- validation fail script
    runContractTagged
      "Lock at script then spend - validation fail"
      testW1
      FailValidation.lockThenSpend
      `andThen` report

    liftIO $ putStrLn "Done. Debug awaiting - Enter to exit" >> void getLine
