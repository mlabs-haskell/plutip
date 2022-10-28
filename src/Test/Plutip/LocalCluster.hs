{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Plutip.LocalCluster (
  BpiWallet,
  RetryDelay,
  addSomeWallet,
  addSomeWalletDir,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  walletPaymentPkh,
  withCluster,
  withConfiguredCluster,
  startCluster,
  stopCluster,
  nodeSocket,
  keysDir,
  sKey,
  payKeys,
  ecSlotLength,
  ClusterTest (ClusterTest),
  WalletSpec (WalletSpec),
  ClusterEnv,
  ExtraConfig (ExtraConfig),
  RunningNode (RunningNode),
  runningNode,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, ask)
import Data.Bifunctor (second)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Numeric.Natural (Natural)
import Numeric.Positive (Positive)
import Test.Plutip.Config (PlutipConfig (extraConfig))

import Test.Plutip.Contract (ClusterTest (ClusterTest), WalletTag (BaseTag, EntTag))
import Test.Plutip.Contract.Types (TestWallet (TestWallet, getWallet))
import Test.Plutip.Internal.BotPlutusInterface.Keys (sKey)
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.BotPlutusInterface.Types (
  AddressType (Base, Enterprise),
  BpiWallet (payKeys),
  WalletSpec (WalletSpec),
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  addSomeWallet,
  addSomeWalletDir,
  cardanoMainnetAddress,
  mkMainnetAddress,
  walletPaymentPkh,
 )
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (ExtraConfig),
  ecSlotLength)
import Test.Plutip.Contract.Types (
  TestWallet (twInitDistribuition),
  TestWallets (unTestWallets),
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types
    ( ClusterEnv(runningNode),
      RunningNode(RunningNode),
      nodeSocket,
      ClusterEnv )
import Test.Plutip.Tools.ChainIndex qualified as CI
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
-- >     [ assertExecution "Get utxos" (initAda (EntTag "w1") 100) (withContract $ const getUtxos) [shouldSucceed]]
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
-- >     [ assertExecution "Get utxos" (initAda (EntTag "w1") 100) (withContract $ const getUtxos) [shouldSucceed]]
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
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [NonEmpty TestWallet])
    setup = do
      env <- ask
      wallets <-
        traverse
          (traverse addTestWallet . getTestWallets)
          testCases
      let waitDelay = ceiling $ ecSlotLength $ extraConfig conf
      awaitFunds wallets waitDelay
      pure (env, wallets)

    getTestWallets (ClusterTest (tws, _)) = tws

    addTestWallet (WalletSpec tag dist _) = do
      bpiW <- addSomeWallet (toAddressType tag) dist
      pure $ TestWallet tag bpiW

    toAddressType :: WalletTag t -> AddressType
    toAddressType = \case
      EntTag _ -> Enterprise
      BaseTag _ -> Base

    awaitFunds ws delay = do
      let lastWallet = NE.last $ last ws
      liftIO $ putStrLn "Waiting till all wallets will be funded to start tests..."
      CI.awaitWalletFunded lastWallet delay

type RetryDelay = Positive

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn = zipWith fn [0 ..]
