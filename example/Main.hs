module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import DebugContract.GetUtxos qualified as GetUtxos
import DebugContract.LockUnlock qualified as LockUnlock
import DebugContract.LockUnlockValidationFail qualified as LockUnlockValidationFail
import DebugContract.PayToWallet qualified as PayToWallet
import Test.Plutip (
  ada,
  addSomeWallet,
  ledgerPaymentPkh,
  runContractWithReport,
  runUsingCluster,
  waitSeconds,
 )


main :: IO ()
main = do
  runUsingCluster $ do
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
