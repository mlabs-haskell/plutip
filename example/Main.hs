module Main (main) where

import DebugContract.GetUtxos qualified as GetUtxos
import DebugContract.LockUnlock qualified as LockUnlock
import DebugContract.LockUnlockValidationFail qualified as LockUnlockValidationFail
import DebugContract.PayToWallet qualified as PayToWallet
import Test.Plutip.Contract (
  ValueOrdering (VLt),
  initAda,
  initAndAssertAda,
  initAndAssertAdaWith,
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
    [ shouldSucceed "Get utxos" (initAndAssertAda 100 100) $ const GetUtxos.getUtxos
    , shouldFail "Throws Contract error" (initAda 100) $ const GetUtxos.getUtxosThrowsErr
    , shouldFail "Throws Exception" (initAda 100) $ const GetUtxos.getUtxosThrowsEx
    , shouldSucceed
        "Pay wallet-to-wallet"
        (initAndAssertAdaWith 300 VLt 290 <> initAndAssertAda 100 110)
        $ \[pkh1] -> PayToWallet.payTo pkh1 10_000_000
    , shouldFail "Lock at script then spend - budget overspend" (initAda 100) $
        const LockUnlock.lockThenSpend
    , shouldFail "Lock at script then spend - validation fail" (initAda 100) $ const LockUnlockValidationFail.lockThenSpend
    ]
