{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Plutip.Contract.Types (
  TestContractConstraints,
  TestContract (..),
  TestWallet (twExpected, twInitDistribuition),
  compareValuesWith,
  ValueOrdering (..),
  Wallets(Nil),
  (+>),
  initTestWallet,
  toList,
  NthWallet(..),
  Predicate(..),
) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Tagged (Tagged (Tagged))
import Fcf.Core (Eval)
import Fcf.Data.List (Length, Snoc, Elem)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Plutus.Contract (AsContractError)
import Test.Plutip.Internal.Types (
  ExecutionResult,
 )
import Test.Tasty.Providers (IsTest (run, testOptions), testFailed, testPassed)

type TestContractConstraints (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat])=
  ( ToJSON w
  , Monoid w
  , Show w
  , Show e
  , Show a
  , Typeable w
  , Typeable e
  , Typeable a
  , Typeable idxs
  , AsContractError e
  )

-- | Test contract
data TestContract (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat])
  = TestContract
      (Predicate w e a idxs)
      -- ^ Info about check to perform and how to report results
      (IO (ExecutionResult w e (a, Wallets idxs Value)))
      -- ^ Result of contract execution
  deriving stock (Typeable)

instance
  forall (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  TestContractConstraints w e a idxs =>
  IsTest (TestContract w e a idxs)
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


type family Insert (xs :: [Nat]) :: [Nat] where
  Insert xs = Eval (Snoc xs (Eval (Length xs)))

data Wallets (idxs :: [Nat]) (w :: Type) where
   Nil  :: Wallets '[] w
   UnsafeCons :: Wallets xs w -> w -> Wallets (Insert xs) w

deriving stock instance (Show w) => Show (Wallets idxs w)

instance Functor (Wallets idxs) where
  fmap _ Nil               = Nil
  fmap f (UnsafeCons ws w) = UnsafeCons (fmap f ws) (f w)

instance Foldable (Wallets idxs) where
  foldMap _ Nil                = mempty
  foldMap f (UnsafeCons ws w)  = foldMap f ws <> f w 

instance Traversable (Wallets idxs) where
  traverse _ Nil               = pure Nil
  traverse f (UnsafeCons ws w) = UnsafeCons <$> traverse f ws <*> f w

(+>) :: IsWallet w => Wallets xs w -> w -> Wallets (Insert xs) w
Nil +> w = UnsafeCons Nil w
ws +> w  = UnsafeCons ws w
infixl 5 +>

toList :: Wallets idxs w -> [w]
toList Nil               = []
toList (UnsafeCons ws w) = toList ws ++ [w]

unsafeIndex :: Wallets xs w -> Int -> w
unsafeIndex (UnsafeCons _ w) 0  = w
unsafeIndex (UnsafeCons ws _) n = unsafeIndex ws (n - 1)
unsafeIndex _ _                 = error "Index out of bounds for Wallets"

class NthWallet (x :: Nat) (xs :: [Nat]) where
  nthWallet :: Wallets xs w -> w

instance (KnownNat n, Eval (Elem n xs) ~ 'True) => NthWallet n xs where
  nthWallet ws = unsafeIndex ws (length ws - fromInteger (natVal @n Proxy) - 1)
  
class IsWallet (w :: Type)

initTestWallet :: [Positive] -> Maybe (ValueOrdering, Value) -> TestWallet
initTestWallet = TestWallet

data TestWallet = TestWallet
  { twInitDistribuition :: [Positive]
  , twExpected :: Maybe (ValueOrdering, Value)
  } deriving stock (Show) -- TODO: for debugging purposes

instance IsWallet TestWallet

data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq
                     deriving stock (Show) -- TODO: for debugging purposes

-- | Predicate is used to build test cases for Contract.
--  List of predicates should be passed to `Test.Plutip.Contract.assertExecution`
--  to make assertions about contract execution.
--  Each predicate will result in separate test case.
--
-- @since 0.2
data Predicate w e a idxs = Predicate
  { -- | description for the case when predicate holds
    positive :: String
  , -- | description for the opposite of `positive` case (mostly for `not` functionality)
    negative :: String
  , -- | some useful debugging info that `Predicate` can print based on contract execution result,
    -- used to print info in case of check failure
    debugInfo :: ExecutionResult w e (a, Wallets idxs Value) -> String
  , -- | check that `Predicate` performs on Contract execution result,
    -- if check evaluates to `False` test case considered failure
    pCheck :: ExecutionResult w e (a, Wallets idxs Value) -> Bool
  }

-- | Value doesn't have an Ord instance, so we cannot use `compare`
compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq
