-- | This module provides some predicates or assertions, that could be used together with
--  `Test.Plutip.Contract.assertExecution` to run tests for Contract in private testnet.
--
--  Module also exports `Predicate` constructor itself, so any arbitrary predicate could be made.
module Test.Plutip.Predicate (
  Predicate (..),
  pTag,
  shouldSucceed,
  Test.Plutip.Predicate.not,
  shouldFail,
  yieldSatisfies,
  shouldYield,
  errorSatisfies,
  failReasonSatisfies,
  shouldThrow,
  stateSatisfies,
  stateIs,
) where

import Data.List.NonEmpty (NonEmpty)
import Ledger (Value)
import Test.Plutip.Internal.Types (
  ExecutionResult (contractState, outcome),
  FailureReason (CaughtException, ContractExecutionError),
  isSuccessful,
 )
import Text.Show.Pretty (ppShow)

-- | Predicate is used to build test cases for Contract.
--  List of predicates should be passed to `Test.Plutip.Contract.assertExecution`
--  to make assertions about contract execution.
--  Each predicate will result in separate test case.
--
-- @since 0.2
data Predicate w e a = Predicate
  { -- | description for the case when predicate holds
    positive :: String
  , -- | description for the opposite of `positive` case (mostly for `not` functionality)
    negative :: String
  , -- | some useful debugging info that `Predicate` can print based on contract execution result,
    -- used to print info in case of check failure
    debugInfo :: ExecutionResult w e (a, NonEmpty Value) -> String
  , -- | check that `Predicate` performs on Contract execution result,
    -- if check evaluates to `False` test case considered failure
    pCheck :: ExecutionResult w e (a, NonEmpty Value) -> Bool
  }

-- | `positive` description of `Predicate` that will be used as test case tag.
--
-- @since 0.2
pTag :: Predicate w e a -> String
pTag = positive

-- | Switch the meaning of `Predicate` to the opposite.
--
-- @since 0.2
not :: Predicate w e a -> Predicate w e a
not predicate =
  let (Predicate pos' neg' dbgInfo' check') = predicate
   in Predicate neg' pos' dbgInfo' (Prelude.not . check')

-- Predefined predicates --

-- Basic success/fail --

-- | Check that Contract didn't fail.
--
-- @since 0.2
shouldSucceed :: (Show e, Show a, Show w) => Predicate w e a
shouldSucceed =
  Predicate
    "Contract should succeed"
    "Contract should fail"
    (mappend "But it didn't.\nResult: " . show)
    isSuccessful

-- | Check that Contract didn't succeed.
--
-- @since 0.2
shouldFail :: (Show e, Show a, Show w) => Predicate w e a
shouldFail = Test.Plutip.Predicate.not shouldSucceed

-- Contract result --

-- | Check that Contract returned the expected value.
--
-- @since 0.2
shouldYield :: (Show a, Eq a) => a -> Predicate w e a
shouldYield expected =
  (yieldSatisfies "" (== expected))
    { positive = "Should yield '" <> ppShow expected <> "'"
    , negative = "Should NOT yield '" <> ppShow expected <> "'"
    }

-- | Check that the returned value of the Contract satisfies the predicate.
--
--  Provided `String` description will be used in tes case tag.
--
-- @since 0.2
yieldSatisfies :: (Show a) => String -> (a -> Bool) -> Predicate w e a
yieldSatisfies description p =
  Predicate
    description
    ("Should violate '" <> description <> "'")
    debugInfo'
    checkOutcome
  where
    debugInfo' r = case outcome r of
      Left _ -> "Contract failed"
      Right (a, _) -> "Got: " <> ppShow a

    checkOutcome r =
      case outcome r of
        Left _ -> False
        Right (a, _) -> p a

-- Contract state --

-- | Check that Contract has expected state after being executed.
--  State will be accessible even if Contract failed.
--
-- @since 0.2
stateIs :: (Show w, Eq w) => w -> Predicate w e a
stateIs expected =
  (stateSatisfies "" (== expected))
    { positive = "State should be '" <> ppShow expected <> "'"
    , negative = "State should NOT be '" <> ppShow expected <> "'"
    }

-- | Check that Contract after execution satisfies the predicate.
--  State will be accessible even if Contract failed.
--
--  Provided `String` description will be used in test case tag.
--
-- @since 0.2
stateSatisfies :: Show w => String -> (w -> Bool) -> Predicate w e a
stateSatisfies description p =
  Predicate
    description
    ("Should violate '" <> description <> "'")
    debugInfo'
    checkState
  where
    currentState = ("Current state is: " <>) . ppShow . contractState
    debugInfo' r = case outcome r of
      Left _ -> "Contract failed.\n" <> currentState r
      Right _ -> currentState r
    checkState r = p (contractState r)

-- Errors --

-- | Check that Contract throws expected error.
--  In case of exception that could happen during Contract execution,
--  predicate won't hold.
--
-- @since 0.2
shouldThrow :: (Show e, Eq e) => e -> Predicate w e a
shouldThrow expected =
  (errorSatisfies "" (== expected))
    { positive = "Should throw '" <> ppShow expected <> "'"
    , negative = "Should NOT throw '" <> ppShow expected <> "'"
    }

-- | Check that error thrown by Contract satisfies predicate.
--  In case of exception that could happen during Contract execution,
--  predicate won't hold.
--
--  Provided `String` description will be used in test case tag.
--
-- @since 0.2
errorSatisfies :: Show e => String -> (e -> Bool) -> Predicate w e a
errorSatisfies description p =
  failReasonSatisfies description $ \case
    ContractExecutionError e -> p e
    _ -> False

-- | The most general check for possible Contract failure.
--  Can examine any possible contract failure represented by `FailureReason`:
--  errors thrown by contracts or exceptions that happened during the run.
--  Provided `String` description will be used in test case tag.
--
-- @since 0.2
failReasonSatisfies :: Show e => String -> (FailureReason e -> Bool) -> Predicate w e a
failReasonSatisfies description p =
  Predicate
    description
    ("Should violate '" <> description <> "'")
    debugInfo'
    checkOutcome
  where
    debugInfo' r = case outcome r of
      Left e -> sayType e <> ppShow e
      Right _ -> "Contract didn't fail"
    checkOutcome r =
      case outcome r of
        Left e -> p e
        Right _ -> False

    sayType = \case
      CaughtException _ -> "Exception was caught: "
      ContractExecutionError _ -> "Error was thrown: "
