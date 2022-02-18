module Main (main) where

import DebugContract.GetUtxos qualified as GetUtxos
import DebugContract.LockUnlock qualified as LockUnlock
import DebugContract.LockUnlockValidationFail qualified as LockUnlockValidationFail
import Test.Plutip.Contract (
  ada,
  -- ledgerPaymentPkh,
  shouldSucceed,
  shouldFail,
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree, defaultMain)
-- import DebugContract.PayToWallet qualified as PayToWallet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  withCluster
    "Integration tests"
    [ada 101, ada 202]
    [ -- 1 successful and 2 failing scenarios
      shouldSucceed "Get utxos" 1 GetUtxos.getUtxos
    , shouldFail "Throws Contract error" 1 GetUtxos.getUtxosThrowsErr
    , shouldFail "Throws Exception" 1 GetUtxos.getUtxosThrowsEx
    , -- , -- successful wallet to wallet transaction
      --   shouldSucceed "Pay wallet-to-wallet" testW1 $
      --     PayToWallet.payTo (ledgerPaymentPkh testW2) 10_000_000
      -- budget overspend script
      shouldFail "Lock at script then spend - budget overspend" 1 LockUnlock.lockThenSpend
    , -- validation fail script
      shouldFail "Lock at script then spend - validation fail" 1 LockUnlockValidationFail.lockThenSpend
    ]
