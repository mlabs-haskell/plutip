module Test.Plutip.LocalCluster (
  BpiWallet,
  addSomeWallet,
  ada,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  withCluster,
  withConfiguredCluster,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Bifunctor (second)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Test.Plutip.Config (PlutipConfig)
import Test.Plutip.Contract (TestWallet (twInitDistribuition), TestWallets (unTestWallets))
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

{- | Spin up a local cluster and create a test group with the contracts inside it.
 The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
 depend on each other (note that time related issues might still occur).
 Uses default `PlutipConfig`.

= Usage
> test :: TestTree
> test =
>   withCluster
>     "Tests with local cluster"
>     [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
>     ...

 @since 0.2
-}
withCluster ::
  String ->
  [(TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)] ->
  TestTree
withCluster = withConfiguredCluster def

{- | Spin up a local cluster and create a test group with the contracts inside it.
 The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
 depend on each other (note that time related issues might still occur).

= Usage
> test :: TestTree
> test =
>     let myConfig = PlutipConfig ...
>     withConfiguredCluster myConfig
>     "Tests with local cluster"
>     [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
>     ...

 @since 0.2
-}
withConfiguredCluster ::
  PlutipConfig ->
  String ->
  [(TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)] ->
  TestTree
withConfiguredCluster conf name testCases =
  withResource (startCluster conf setup) (stopCluster . fst) $
    \getResource ->
      testGroup name $
        imap
          (\idx (_, toTestCase) -> toTestCase $ second (!! idx) . snd <$> getResource)
          testCases
  where
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty BpiWallet])
    setup = do
      env <- ask

      wallets <-
        traverse
          (traverse addSomeWallet . fmap twInitDistribuition . unTestWallets . fst)
          testCases
      waitSeconds 2 -- wait for transactions to submit
      pure (env, wallets)

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn = zipWith fn [0 ..]
