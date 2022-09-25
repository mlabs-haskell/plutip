{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
  WalletType (..),
  WalletTag (..),
  -- WalletInfo (..),ownPaymentPubKeyHash, ownStakePubKeyHash, ownAddress,
  -- WalletTypeError(..),
  -- WalletLookups, lookupAddress, lookupWallet, WalletInfo'(..)
) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Tagged (Tagged (Tagged))
import Ledger.Value (Value)
import Plutus.Contract (AsContractError)
import Test.Plutip.Internal.BotPlutusInterface.Types (WalletTag (EnterpriseTag, WithStakeKeysTag), WalletType (Enterprise, WithStakeKeys))
import Test.Plutip.Internal.Types (
  ExecutionResult,
 )
import Test.Plutip.Predicate (Predicate, debugInfo, pCheck)
import Test.Tasty.Providers (IsTest (run, testOptions), testFailed, testPassed)

type TestContractConstraints (w :: Type) (e :: Type) (k :: Type) (a :: Type) =
  ( ToJSON w
  , Monoid w
  , Show w
  , Show e
  , Show a
  , Show k
  , Typeable w
  , Typeable e
  , Typeable k
  , Typeable a
  , Ord k
  , AsContractError e
  )

-- | Test contract
data TestContract (w :: Type) (e :: Type) (k :: Type) (a :: Type)
  = TestContract
      (Predicate w e k a)
      -- ^ Info about check to perform and how to report results
      (IO (ExecutionResult w e (a, Map k Value)))
      -- ^ Result of contract execution
  deriving stock (Typeable)

instance
  forall (w :: Type) (e :: Type) (k :: Type) (a :: Type).
  TestContractConstraints w e k a =>
  IsTest (TestContract w e k a)
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
