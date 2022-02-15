{-# LANGUAGE ImplicitParams #-}

--
module Test.Plutip (
  BpiWallet,
  addSomeWallet,
  runContractTagged,
  runContract,
  runContract_,
  runUsingCluster,
  ada,
  waitSeconds,
  report,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  andThen,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Text.IO qualified as TIO
import Numeric.Natural (Natural)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
import Test.Plutip.BotPlutusInterface.Run (runContract, runContractTagged, runContract_)
import Test.Plutip.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Test.Plutip.LocalCluster.Cluster (runUsingCluster)
import Test.Plutip.LocalCluster.Types (ClusterEnv, RunResult, isSuccess, prettyResult)
import Test.Plutip.Tools (ada)
import Test.Tasty.Ingredients.ConsoleReporter (withConsoleFormat)
import Test.Tasty.Providers.ConsoleFormat (failFormat, okFormat)

-- | Print contract execution result to terminal
report :: (Show a, Show w, Show e, MonadIO m) => RunResult w e a -> m ()
report r = liftIO $ do
  canColors <- hSupportsANSIColor stdout
  let ?colors = canColors
  withConsoleFormat
    (pickFormat r)
    (TIO.putStrLn $ prettyResult r)
  where
    pickFormat res =
      if isSuccess res
        then okFormat
        else failFormat

-- | Awaiting via `threadDelay`
waitSeconds :: Natural -> ReaderT ClusterEnv IO ()
waitSeconds n = liftIO $ threadDelay (fromEnum n * 1_000_000)

-- | Alias for `>>=` for readability
andThen :: Monad m => m a -> (a -> m b) -> m b
andThen = (>>=)
