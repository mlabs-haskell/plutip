{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

--
module Test.Plutip (
  BpiWallet,
  addSomeWallet,
  shouldSucceed,
  runContract,
  runContract_,
  ada,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  withCluster,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (ToJSON)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import Numeric.Natural (Natural)
import Plutus.Contract (Contract)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract, runContract_)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Test.Plutip.Internal.LocalCluster.Cluster (runUsingCluster)
import Test.Plutip.Internal.LocalCluster.Types (ClusterEnv, isSuccess)
import Test.Plutip.Tools (ada)
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testFailed, testPassed)

-- | Awaiting via `threadDelay`
waitSeconds :: Natural -> ReaderT ClusterEnv IO ()
waitSeconds n = liftIO $ threadDelay (fromEnum n * 1_000_000)

withCluster ::
  String ->
  -- TODO: better data type for cluster config
  [Natural] ->
  [IO (ClusterEnv, [BpiWallet]) -> TestTree] ->
  TestTree
withCluster name walletAmts testCases =
  -- TODO: split up withCluster into startCluster and stopCluster
  withResource (runUsingCluster setup) (const (pure ())) $
    \getResource -> testGroup name $ map (\t -> t getResource) testCases
  where
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet])
    setup = do
      env <- ask
      wallets <- traverse addSomeWallet walletAmts
      waitSeconds 2 -- wait for transactions to submit
      pure (env, wallets)

shouldSucceed ::
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  String ->
  Int ->
  Contract w s e a ->
  IO (ClusterEnv, [BpiWallet]) ->
  TestTree
shouldSucceed t walletIdx c =
  singleTest t . TestContract walletIdx c

data TestContract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) = TestContract
  { tcWallet :: Int
  , tcContract :: (ToJSON w, Monoid w, Show w, Show e, Show a) => Contract w s e a
  , tcSetup :: IO (ClusterEnv, [BpiWallet])
  }
  deriving stock (Typeable)

instance
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  IsTest (TestContract w s e a)
  where
  run _ TestContract {tcWallet, tcContract, tcSetup} _ = do
    (cEnv, wallets) <- tcSetup
    -- TODO: this is unsafe
    let wallet = wallets !! tcWallet
    res <- runReaderT (runContract cEnv wallet tcContract) cEnv

    if isSuccess res
      then pure $ testPassed $ show res
      else pure $ testFailed $ show res

  testOptions = Tagged []
