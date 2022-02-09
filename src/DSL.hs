module DSL (
  BpiWallet,
  RunResult (RunSuccess, RunFailed),
  addSomeWallet,
  runContract,
  runContract_,
  runUsingCluster,
  runUsingCluster',
  ada,
  waitSeconds,
  report,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
) where

import BotInterface.Run (runContract, runContract_)
import BotInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Control.Monad.IO.Class (MonadIO, liftIO)
import LocalCluster.Cluster (runUsingCluster, runUsingCluster')
import LocalCluster.Types (RunResult (RunFailed, RunSuccess))
import Utils (ada, waitSeconds)

{- | Stand-in for upcoming report functionality
 (just print out for now)
-}
report :: (Show a, MonadIO m) => a -> m ()
report = liftIO . print
