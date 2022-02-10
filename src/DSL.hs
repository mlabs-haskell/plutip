{-# LANGUAGE ImplicitParams #-}

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
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Ingredients.ConsoleReporter (withConsoleFormat)
import Test.Tasty.Providers.ConsoleFormat (failFormat, okFormat)

{- | Stand-in for upcoming report functionality
 (just print out for now)
-}
report :: (Show a, Show w, Show e, MonadIO m) => RunResult w e a -> m ()
-- report = liftIO . print
report r = do
    let ?colors = True 
    liftIO $ withConsoleFormat (pickFormat r) (prettyPrint r)
    where
      prettyPrint = print
      pickFormat = \case
        RunSuccess _ _ -> okFormat
        RunFailed _ -> failFormat
    
