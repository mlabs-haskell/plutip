{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

--
module Test.Plutip.LocalCluster (
  BpiWallet,
  addSomeWallet,
  ada,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  withCluster,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Numeric.Natural (Natural)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.Tools (ada)
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Providers (TestTree)

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
  withResource (startCluster setup) (stopCluster . fst) $
    \getResource -> testGroup name $ map (\t -> t (snd <$> getResource)) testCases
  where
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet])
    setup = do
      env <- ask
      wallets <- traverse addSomeWallet walletAmts
      waitSeconds 2 -- wait for transactions to submit
      pure (env, wallets)
