{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

--
module Test.Plutip.Contract (
  shouldSucceed,
  shouldFail,
  ada,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
) where

import BotPlutusInterface.Types (ContractState)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (ToJSON)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import Plutus.Contract (Contract)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  mkMainnetAddress,
 )
import Test.Plutip.Internal.Types (ClusterEnv, FailureReason, Outcome (Failure, Success))
import Test.Plutip.Tools (ada)
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testFailed, testPassed)

shouldSucceed ::
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  String ->
  Int ->
  Contract w s e a ->
  IO (ClusterEnv, [BpiWallet]) ->
  TestTree
shouldSucceed t walletIdx c =
  singleTest t . TestContract walletIdx c (ExpectSuccess Nothing Nothing)

shouldFail ::
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  String ->
  Int ->
  Contract w s e a ->
  IO (ClusterEnv, [BpiWallet]) ->
  TestTree
shouldFail t walletIdx c =
  singleTest t . TestContract walletIdx c (ExpectFailure Nothing)

data TestContract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) = TestContract
  { tcWallet :: Int
  , tcContract :: (ToJSON w, Monoid w, Show w, Show e, Show a) => Contract w s e a
  , tcExpected :: ExpectedOutcome w e a
  , tcSetup :: IO (ClusterEnv, [BpiWallet])
  }
  deriving stock (Typeable)

-- | Outcome of running contract
data ExpectedOutcome w e a
  = ExpectSuccess
      { -- | return value of `Contract`
        contractResult :: Maybe a
      , -- | `Contract` state after execution
        contractState :: Maybe (ContractState w)
      }
  | ExpectFailure
      { -- | reason of `Contract` execution failure
        reason :: Maybe (FailureReason e)
      }
  deriving stock (Typeable)

instance
  (ToJSON w, Monoid w, Show w, Show e, Show a, Typeable s, Typeable w, Typeable e, Typeable a) =>
  IsTest (TestContract w s e a)
  where
  run _ TestContract {tcWallet, tcContract, tcSetup, tcExpected} _ = do
    (cEnv, wallets) <- tcSetup
    -- TODO: this is unsafe
    let wallet = wallets !! tcWallet
    result <- runReaderT (runContract cEnv wallet tcContract) cEnv

    pure $ case (tcExpected, result) of
      (ExpectSuccess _ _, Success _ _) -> testPassed $ show result
      (ExpectFailure _, Failure _) -> testPassed $ show result
      _ -> testFailed $ show result

  testOptions = Tagged []
