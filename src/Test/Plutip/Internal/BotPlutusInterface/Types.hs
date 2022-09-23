{-# LANGUAGE GADTs #-}
module Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiError (..),
  BpiWallet (BpiWallet, payKeys, stakeKeys),
  TestWallet (TestWallet, twInitDistribiution, twExpected, testWalletTag),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq, VGt, VLt, VGEq, VLEq),
  compareValuesWith,
  WalletType(..),
  WalletTag(..),
  TestWallet'(..), 
) where

import Data.List.NonEmpty (NonEmpty)
import Ledger (Value)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair, StakeKeyPair)

data WalletTag t k where
  WithStakeKeysTag :: k -> WalletTag 'WithStakeKeys k
  EnterpriseTag :: k -> WalletTag 'Enterprise k

deriving stock instance Show k => Show (WalletTag t k)
deriving stock instance Eq k => Eq (WalletTag t k)

data WalletType
  = Enterprise
  | WithStakeKeys
  deriving stock (Show, Eq)

data BpiError
  = SignKeySaveError !String
  | BotInterfaceDirMissing
  deriving stock (Show, Eq)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet k = BpiWallet
  { payKeys :: KeyPair
  , stakeKeys :: Maybe StakeKeyPair
  , bpiWalletTag :: k
  }
  deriving stock (Show)

newtype TestWallets k = TestWallets {unTestWallets :: NonEmpty (TestWallet' k) }
  deriving newtype (Semigroup)

newtype TestWallet' k = TestWallet' (forall t . TestWallet t k)

data TestWallet t k = TestWallet
  { twInitDistribiution :: [Positive]
  , twExpected :: Maybe (ValueOrdering, Value)
  , testWalletTag :: WalletTag t k
  }

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq
