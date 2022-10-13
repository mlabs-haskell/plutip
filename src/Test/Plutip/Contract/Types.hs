{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
  WalletTag (..),
) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Ledger.Value (Value)
import Plutus.Contract (AsContractError)
import Test.Plutip.Internal.BotPlutusInterface.Types (WalletTag (BaseTag, EntTag))
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
      (IO (ExecutionResult w e (a, Map Text Value)))
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
