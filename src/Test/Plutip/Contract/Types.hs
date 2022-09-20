{-# LANGUAGE ConstraintKinds #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
  TestWallets (TestWallets, unTestWallets),
  TestWallet (..),
  compareValuesWith,
  ValueOrdering (..),
  WalletInfo (..),
  makeWalletInfo,
) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Tagged (Tagged (Tagged))
import Ledger (Address, PaymentPubKeyHash, StakePubKeyHash, pubKeyHashAddress)
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Plutus.Contract (AsContractError)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  walletPaymentPkh,
  walletStakePkh,
 )
import Test.Plutip.Internal.Types (
  ExecutionResult,
 )
import Test.Plutip.Predicate (Predicate, debugInfo, pCheck)
import Test.Tasty.Providers (IsTest (run, testOptions), testFailed, testPassed)

type TestContractConstraints (w :: Type) (e :: Type) (a :: Type) =
  ( ToJSON w
  , Monoid w
  , Show w
  , Show e
  , Show a
  , Typeable w
  , Typeable e
  , Typeable a
  , AsContractError e
  )

-- | Test contract
data TestContract (w :: Type) (e :: Type) (a :: Type)
  = TestContract
      (Predicate w e a)
      -- ^ Info about check to perform and how to report results
      (IO (ExecutionResult w e (a, NonEmpty Value)))
      -- ^ Result of contract execution
  deriving stock (Typeable)

instance
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  IsTest (TestContract w e a)
  where
  run _ (TestContract predicate runResult) _ = do
    result <- runResult
    pure $
      bool
        ( testFailed $
            debugInfo predicate result
              <> "\n\n"
              <> "Use assertExecutionWith to show contract logs or budgets."
        )
        (testPassed "")
        (pCheck predicate result)

  testOptions = Tagged []

newtype TestWallets = TestWallets {unTestWallets :: NonEmpty TestWallet}
  deriving newtype (Semigroup)

data TestWallet = TestWallet
  { twInitDistribiution :: [Positive]
  , twExpected :: Maybe (ValueOrdering, Value)
  , hasStakeKeys :: Bool
  }

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

data WalletInfo = WalletInfo
  { ownAddress :: Address
  , ownPaymentPubKeyHash :: PaymentPubKeyHash
  , ownStakePubKeyHash :: Maybe StakePubKeyHash
  }

makeWalletInfo :: BpiWallet -> WalletInfo
makeWalletInfo wall = WalletInfo addr pkh spkh
  where
    addr = pubKeyHashAddress pkh spkh
    pkh = walletPaymentPkh wall
    spkh = walletStakePkh wall
