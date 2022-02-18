{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

--
module Test.Plutip (
  BpiWallet,
  addSomeWallet,
  shouldSucceed,
  shouldFail,
  runContract,
  runContract_,
  ada,
  waitSeconds,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  withCluster,
) where

import BotPlutusInterface.Types (ContractState)
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
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.LocalCluster.Types (ClusterEnv, FailReason, isSuccess)
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
  withResource (startCluster setup) (stopCluster . fst) $
    \getResource -> testGroup name $ map (\t -> t (snd <$> getResource)) testCases
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
  singleTest t . TestContract walletIdx c (Success Nothing Nothing)

shouldFail ::
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  String ->
  Int ->
  Contract w s e a ->
  IO (ClusterEnv, [BpiWallet]) ->
  TestTree
shouldFail t walletIdx c =
  singleTest t . TestContract walletIdx c (Fail Nothing)

data TestContract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) = TestContract
  { tcWallet :: Int
  , tcContract :: (ToJSON w, Monoid w, Show w, Show e, Show a) => Contract w s e a
  , tcExpect :: ExpectedOutcome w e a
  , tcSetup :: IO (ClusterEnv, [BpiWallet])
  }
  deriving stock (Typeable)

-- | Outcome of running contract
data ExpectedOutcome w e a
  = Success
      { -- | return value of `Contract`
        contractResult :: Maybe a
      , -- | `Contract` state after execution
        contractState :: Maybe (ContractState w)
      }
  | Fail
      { -- | reason of `Contract` execution failure
        reason :: Maybe (FailReason e)
      }
  deriving stock (Typeable)

instance
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  IsTest (TestContract w s e a)
  where
  run _ TestContract {tcWallet, tcContract, tcSetup, tcExpect} _ = do
    (cEnv, wallets) <- tcSetup
    -- TODO: this is unsafe
    let wallet = wallets !! tcWallet
    res <- runReaderT (runContract cEnv wallet tcContract) cEnv

    case tcExpect of
      Success _ _ ->
        if isSuccess res
          then pure $ testPassed $ show res
          else pure $ testFailed $ show res
      Fail _ ->
        if not (isSuccess res)
          then pure $ testPassed $ show res
          else pure $ testFailed $ show res

  testOptions = Tagged []
