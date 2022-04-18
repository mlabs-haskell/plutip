{-# LANGUAGE ConstraintKinds #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
  TestWallets (TestWallets, unTestWallets),
  TestWallet (..),
  compareValuesWith,
  ValueOrdering (..),
) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Tagged (Tagged (Tagged))
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Plutus.Contract (AsContractError)
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
        (testFailed $ debugInfo predicate result)
        (testPassed "")
        (pCheck predicate result)

  testOptions = Tagged []

newtype TestWallets = TestWallets {unTestWallets :: NonEmpty TestWallet}
  deriving newtype (Semigroup)

data TestWallet = TestWallet
  { twInitDistribuition :: Positive
  , twExpected :: Maybe (ValueOrdering, Value)
  }

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq
