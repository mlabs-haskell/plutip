module Main (main) where

import DebugContract.GetUtxos qualified as GetUtxos
import DebugContract.LockUnlock qualified as LockUnlock
import DebugContract.LockUnlockValidationFail qualified as LockUnlockValidationFail
import DebugContract.PayToWallet qualified as PayToWallet
import Test.Plutip.Contract (
  ada,
  ledgerPaymentPkh,
  shouldFail,
  shouldSucceed,
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  withCluster
    "Integration tests"
    [ shouldSucceed "Get utxos" (ada 100) $ const GetUtxos.getUtxos
    , shouldFail "Throws Contract error" (ada 100) $ const GetUtxos.getUtxosThrowsErr
    , shouldFail "Throws Exception" (ada 100) $ const GetUtxos.getUtxosThrowsEx
    , shouldSucceed "Pay wallet-to-wallet" (ada 300 <> ada 200) $ \[w1] ->
        PayToWallet.payTo (ledgerPaymentPkh w1) 10_000_000
    , shouldFail "Lock at script then spend - budget overspend" (ada 100) $
        const LockUnlock.lockThenSpend
    , shouldFail "Lock at script then spend - validation fail" (ada 100) $ const LockUnlockValidationFail.lockThenSpend
    ]
