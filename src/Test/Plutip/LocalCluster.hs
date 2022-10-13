{-# LANGUAGE ScopedTypeVariables #-}

module Test.Plutip.LocalCluster (
  BpiWallet,
  addSomeWallet,
  ada,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  walletPaymentPkh,
  withCluster,
  withConfiguredCluster,
  startCluster,
  stopCluster,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Bifunctor (second)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Test.Plutip.Config (PlutipConfig)
import Test.Plutip.Contract (ClusterTest (ClusterTest), ada)
import Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiWallet,
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  addSomeWallet,
  cardanoMainnetAddress,
  mkMainnetAddress,
  walletPaymentPkh,
 )
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Providers (TestTree)

-- | Awaiting via `threadDelay`
waitSeconds :: Natural -> ReaderT ClusterEnv IO ()
waitSeconds n = liftIO $ threadDelay (fromEnum n * 1_000_000)

-- | Spin up a local cluster and create a test group with the contracts inside it.
-- The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
-- depend on each other (note that time related issues might still occur).
-- Uses default `PlutipConfig`.
--
-- = Usage
-- > test :: TestTree
-- > test =
-- >   withCluster
-- >     "Tests with local cluster"
-- >     [ assertExecution "Get utxos" (initAda (PkhTag ()) 100) (withContract $ const getUtxos) [shouldSucceed]]
-- >     ...
--
-- @since 0.2
withCluster ::
  String ->
  [ClusterTest] ->
  TestTree
withCluster = withConfiguredCluster def

-- | Spin up a local cluster and create a test group with the contracts inside it.
-- The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
-- depend on each other (note that time related issues might still occur).
--
-- = Usage
-- > test :: TestTree
-- > test =
-- >     let myConfig = PlutipConfig ...
-- >     withConfiguredCluster myConfig
-- >     "Tests with local cluster"
-- >     [ assertExecution "Get utxos" (initAda (PkhTag ()) 100) (withContract $ const getUtxos) [shouldSucceed]]
-- >     ...
--
-- @since 0.2
withConfiguredCluster ::
  PlutipConfig ->
  String ->
  [ClusterTest] ->
  TestTree
withConfiguredCluster conf name testCases =
  withResource (startCluster conf setup) (stopCluster . fst) $
    \getResource ->
      testGroup name $
        imap
          (\idx (ClusterTest (_, toTestGroup)) -> toTestGroup $ second (!! idx) . snd <$> getResource)
          testCases
  where
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty BpiWallet])
    setup = do
      env <- ask

      wallets <-
        traverse
          (traverse addSomeWallet . getTestWallets)
          testCases
      -- had to bump waiting period here coz of chain-index slowdown,
      -- see https://github.com/mlabs-haskell/plutip/issues/120
      waitSeconds 5 -- wait for transactions to submit
      pure (env, wallets)

    getTestWallets (ClusterTest (tws, _)) = tws

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn = zipWith fn [0 ..]
