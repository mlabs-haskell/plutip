{-# LANGUAGE GADTs #-}

module Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiError (..),
  BpiWallet (BpiWallet, payKeys, stakeKeys),
  ValueOrdering (VEq, VGt, VLt, VGEq, VLEq),
  compareValuesWith,
  WalletTag (..),
  WalletSpec (..),
  WalletInfo,
  ownPaymentPubKeyHash,
  ownStakePubKeyHash,
  ownAddress,
  getTag,
  BaseWallet (..),
  EntWallet (..),
  AddressType (..),
) where

import Cardano.Api (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (Address, PaymentPubKeyHash, StakePubKeyHash, Value, pubKeyHashAddress)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair, StakeKeyPair)

-- |Type of address for wallet creation.
-- Currently Base and Enterprise addresses are supported.
data AddressType
  = -- | Option to create wallet with Base Address
    -- (with verification and staking keys)
    Base
  | -- | Option to create wallet with Enterprise Address
    -- (with verification keys only keys)
    Enterprise
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Tag of the wallet that gives wallet name and specifies type of address it will have:
-- base or enterprise.
-- Also used in lookups and specifies expected type of returned wallet.
--
-- `t` type parameter is the type of wallet that will be accessible from `WalletLookups`.
data WalletTag t where
  -- | Option to create wallet with base address: has both payment and staking keys
  BaseTag :: Text -> WalletTag BaseWallet
  -- | Option to create wallet with enterprise address: has only payment keys
  EntTag :: Text -> WalletTag EntWallet

deriving stock instance Show (WalletTag t)
deriving stock instance Eq (WalletTag t)

data BpiError
  = SignKeySaveError !String
  | BotInterfaceDirMissing
  deriving stock (Show, Eq)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet = BpiWallet
  { payKeys :: KeyPair
  , stakeKeys :: Maybe StakeKeyPair
  }
  deriving stock (Show)

data WalletSpec = forall t.
  WalletSpec
  { wsTag :: WalletTag t
  , wsDistribution :: [Positive]
  , wsExpected :: Maybe (ValueOrdering, Value)
  }

getTag :: WalletSpec -> Text
getTag (WalletSpec tag _ _) = getTag' tag
  where
    getTag' :: WalletTag t -> Text
    getTag' = \case
      BaseTag tag' -> tag'
      EntTag tag' -> tag'

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

-- | Type holding wallet information as seen with wallet lookups. Used internally only.
type WalletInfo = Either BaseWallet EntWallet

-- | Base address wallet: supported by both Payment and Staking keys.
data BaseWallet = BaseWallet {getBasePkh :: PaymentPubKeyHash, getSpkh :: StakePubKeyHash}
  deriving stock (Eq, Show, Typeable)

-- | Enterprise address wallet: supported only by Payment keys.
newtype EntWallet = EntWallet {getPkh :: PaymentPubKeyHash}
  deriving stock (Eq, Show, Typeable)

ownPaymentPubKeyHash :: WalletInfo -> PaymentPubKeyHash
ownPaymentPubKeyHash = either getBasePkh getPkh

ownStakePubKeyHash :: WalletInfo -> Maybe StakePubKeyHash
ownStakePubKeyHash = either (Just . getSpkh) (const Nothing)

ownAddress :: WalletInfo -> Address
ownAddress w = pubKeyHashAddress (ownPaymentPubKeyHash w) (ownStakePubKeyHash w)
