{-# LANGUAGE GADTs #-}

module Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiError (..),
  BpiWallet (BpiWallet, payKeys, stakeKeys, bwTag),
  TestWallet (TestWallet, twInitDistribiution, twExpected, twTag),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq, VGt, VLt, VGEq, VLEq),
  compareValuesWith,
  WalletTag (..),
  TestWallet' (..),
  WalletInfo,
  ownPaymentPubKeyHash,
  ownStakePubKeyHash,
  ownAddress,
  getTag,
  -- WalletInfo' (..),
  testWallet',
  SomeBpiWallet (..),
  SomeTestWallet' (..),
  BaseWallet (..),
  PkhWallet (..),
) where

import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Ledger (Address, PaymentPubKeyHash, StakePubKeyHash, Value, pubKeyHashAddress)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair, StakeKeyPair)

-- | Name for the wallet (k) together with information on what we expect the wallet to be.
-- Used in wallet initialization specifies requested wallet's type, used in lookups specifies expected returned wallet type.
--
-- Don't use the same name `k` for two wallets, even with different tag constructors.
-- `t` type parameter is the type of wallet that will be accessible from WalletLookups.
data WalletTag t k where
  -- | Base address wallet: has both payment and staking keys
  BaseTag :: k -> WalletTag BaseWallet k
  -- | Enterprise address wallet: has only payment keys
  PkhTag :: k -> WalletTag PkhWallet k

deriving stock instance Show k => Show (WalletTag t k)
deriving stock instance Eq k => Eq (WalletTag t k)

getTag :: WalletTag t k -> k
getTag = \case
  BaseTag k -> k
  PkhTag k -> k

data BpiError
  = SignKeySaveError !String
  | BotInterfaceDirMissing
  deriving stock (Show, Eq)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet k = BpiWallet
  { payKeys :: KeyPair
  , stakeKeys :: Maybe StakeKeyPair
  , bwTag :: k
  }
  deriving stock (Show)

data SomeBpiWallet = forall k. SomeBpiWallet (BpiWallet k)

-- | Test wallets with k typed wallet tags.
newtype TestWallets k = TestWallets {unTestWallets :: NonEmpty (TestWallet' k)}
  deriving newtype (Semigroup)

data TestWallet' k = forall t. TestWallet' (TestWallet t k)

-- | Make TestWallet', takes utxo distribution, value assertions and WalletTag as arguments.
testWallet' :: [Positive] -> Maybe (ValueOrdering, Value) -> WalletTag t k -> TestWallet' k
testWallet' twInitDistribiution twExpected twTag = TestWallet' $ TestWallet twInitDistribiution twExpected twTag

data SomeTestWallet' = forall k. SomeTestWallet' (TestWallet' k)

-- | Description of wallet to initialize
data TestWallet t k = TestWallet
  { twInitDistribiution :: [Positive]
  , twExpected :: Maybe (ValueOrdering, Value)
  , twTag :: WalletTag t k
  }

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

-- | Type holding wallet information as seen with wallet lookups. Used internally only.
type WalletInfo = Either BaseWallet PkhWallet

-- | Base address wallet: supported by both Payment and Staking keys.
data BaseWallet = BaseWallet {getBasePkh :: PaymentPubKeyHash, getSpkh :: StakePubKeyHash}
  deriving stock (Eq, Show, Typeable)

-- | Enterprise address wallet: supported only by Payment keys.
newtype PkhWallet = PkhWallet {getPkh :: PaymentPubKeyHash}
  deriving stock (Eq, Show, Typeable)

ownPaymentPubKeyHash :: WalletInfo -> PaymentPubKeyHash
ownPaymentPubKeyHash = either getBasePkh getPkh

ownStakePubKeyHash :: WalletInfo -> Maybe StakePubKeyHash
ownStakePubKeyHash = either (Just . getSpkh) (const Nothing)

ownAddress :: WalletInfo -> Address
ownAddress w = pubKeyHashAddress (ownPaymentPubKeyHash w) (ownStakePubKeyHash w)
