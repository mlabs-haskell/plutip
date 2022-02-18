module Main (main) where

import DebugContract.GetUtxos qualified as GetUtxos
import DebugContract.LockUnlock qualified as LockUnlock
import Data.List.NonEmpty ((:|))
import DebugContract.LockUnlockValidationFail qualified as LockUnlockValidationFail
import Test.Plutip.Contract (
  ada,

  ledgerPaymentPkh,
  shouldSucceed,
  shouldFail,
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree, defaultMain)
import DebugContract.PayToWallet qualified as PayToWallet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  withCluster
    "Integration tests"
    [ shouldSucceed "Get utxos" (ada 100) $ \_ -> GetUtxos.getUtxos
    , shouldFail "Throws Contract error" (ada 100) $ \_ -> GetUtxos.getUtxosThrowsErr
    , shouldFail "Throws Exception" (ada 100) $ \_ -> GetUtxos.getUtxosThrowsEx
    , shouldSucceed "Pay wallet-to-wallet" (ada 100 <> ada 200) $ \[pkh1] ->
          PayToWallet.payTo pkh1 10_000_000
      
    , shouldFail "Lock at script then spend - budget overspend" (ada 100) 
              $ \_ -> LockUnlock.lockThenSpend
    , 
      shouldFail "Lock at script then spend - validation fail" (ada 100) $ \_ -> LockUnlockValidationFail.lockThenSpend
    ]
