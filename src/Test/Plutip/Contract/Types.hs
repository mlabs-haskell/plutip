{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
  WalletInfo (..),
  WalletType(..),
  WalletTag(..),
WalletLookups, lookupAddress, lookupWallet, ownPaymentPubKeyHash, ownStakePubKeyHash, ownAddress) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Tagged (Tagged (Tagged))
import Ledger (Address, PaymentPubKeyHash, StakePubKeyHash, pubKeyHashAddress)
import Ledger.Value (Value)
import Plutus.Contract (AsContractError, Contract)
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
import Data.Row (Row)
import Plutus.Contract.Types (AsContractError(_ContractError))
import Control.Lens.Prism (_Right)

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

data WalletTypeError
  = -- | Expected enterprise address wallet, got one with staking keys.
    ExpectedEnterpriseWallet
    -- | Expected base address wallet, got one without staking keys.
  | ExpectedWalletWithStakeKeys
    -- | Index outside of range
  | BadWalletIndex

instance AsContractError e => AsContractError (Either WalletTypeError e) where
  _ContractError = _Right . _ContractError

instance Show WalletTypeError where
  show ExpectedEnterpriseWallet = "Expected base address wallet, got one with staking keys."
  show ExpectedWalletWithStakeKeys = "Expected base address wallet, got one with staking keys."
  show BadWalletIndex = "Index outside of range."

data WalletInfo t where
  WithStakeKeysInfo :: PaymentPubKeyHash -> StakePubKeyHash -> WalletInfo 'WithStakeKeys
  EnterpriseInfo :: PaymentPubKeyHash -> WalletInfo 'Enterprise

ownPaymentPubKeyHash :: WalletInfo t -> PaymentPubKeyHash
ownPaymentPubKeyHash = \case 
  WithStakeKeysInfo pkh _ -> pkh
  EnterpriseInfo pkh -> pkh

ownStakePubKeyHash :: WalletInfo t -> Maybe StakePubKeyHash
ownStakePubKeyHash = \case
  WithStakeKeysInfo _ spkh -> Just spkh
  EnterpriseInfo _ -> Nothing

ownAddress :: WalletInfo t -> Address 
ownAddress w = pubKeyHashAddress (ownPaymentPubKeyHash w) (ownStakePubKeyHash w)

data WalletInfo' = forall t . WalletInfo' { getWalletInfo :: WalletInfo t}

data WalletLookups k = WalletLookups {
  lookupWallet :: 
    forall (w :: Type) (s :: Row Type) (e :: Type) (t :: WalletType) .
    MonadError (Either WalletTypeError e) m =>
    WalletTag t k
    -> m (WalletInfo t)
  , 
  lookupAddress ::
    forall (w :: Type) (s :: Row Type) (e :: Type) (t :: Type) . 
    MonadError (Either WalletTypeError e) m =>
    k
    -> m (Either WalletTypeError e) Address
}
