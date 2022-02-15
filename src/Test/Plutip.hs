{-# LANGUAGE ImplicitParams #-}

--
module Test.Plutip (
  BpiWallet,
  addSomeWallet,
  runContractTagged,
  runContract,
  runContractWithReport,
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
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Numeric.Natural (Natural)
import Plutus.Contract (Contract)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract, runContractTagged, runContract_)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Test.Plutip.Internal.LocalCluster.Cluster (runUsingCluster)
import Test.Plutip.Internal.LocalCluster.Types (ClusterEnv, RunResult, isSuccess, prettyResult)
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

runContractWithReport ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, Show w, Show e, Show a, MonadIO m, MonadCatch m) =>
  Text ->
  BpiWallet ->
  Contract w s e a ->
  ReaderT ClusterEnv m ()
runContractWithReport t w c = runContractTagged t w c >>= report

-- | Alias for `>>=` for readability
andThen :: Monad m => m a -> (a -> m b) -> m b
andThen = (>>=)
