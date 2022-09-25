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
import Test.Plutip.Contract (ada, ClusterTest(ClusterTest))
import Test.Plutip.Internal.BotPlutusInterface.Types (TestWallets (unTestWallets, TestWallets), SomeBpiWallet (SomeBpiWallet), BpiWallet (bwTag), TestWallet (twTag), getTag, TestWallet' (TestWallet'), SomeTestWallet' (SomeTestWallet'))
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
import qualified Data.List.NonEmpty as NonEmpty

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
-- >     [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
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
-- >     [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
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
          (\idx (ClusterTest (tws, toTestGroup)) -> toTestGroup $ second (substituteTags tws . (!! idx)) . snd <$> getResource)
          testCases
  where
    -- setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty SomeBpiWallet])
    setup = do
      env <- ask

      wallets <-
        traverse
          (traverse (\(SomeTestWallet' tw) -> SomeBpiWallet <$> addSomeWallet tw) . getSomeTestWallets)
          testCases
      -- had to bump waiting period here coz of chain-index slowdown,
      -- see https://github.com/mlabs-haskell/plutip/issues/120
      waitSeconds 5 -- wait for transactions to submit
      pure (env, wallets)

    getSomeTestWallets (ClusterTest (tws, _)) = SomeTestWallet' <$> unTestWallets tws

    -- | Restore type information on BpiWallets by substituting tags from matching test wallets.
    substituteTags :: TestWallets k -> NonEmpty SomeBpiWallet -> NonEmpty (BpiWallet k)
    substituteTags (TestWallets tws) walls =
      NonEmpty.zipWith (\(TestWallet' tw) (SomeBpiWallet bw) -> bw {bwTag = getTag (twTag tw)}) tws walls

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn = zipWith fn [0 ..]
