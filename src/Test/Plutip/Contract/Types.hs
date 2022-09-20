{-# LANGUAGE ConstraintKinds #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
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
