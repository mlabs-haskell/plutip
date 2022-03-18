module Test.Plutip.Predicate
  ( Predicate (..),
    shouldSucceed,
    Test.Plutip.Predicate.not,
    shouldFail,
    yieldSatisfies,
    shouldYield,
    errorSatisfies,
  shouldThrow)
where

import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty)
import Ledger (Value)
import Test.Plutip.Internal.Types (ExecutionResult (outcome), FailureReason (ContractExecutionError))

data Predicate w e a = Predicate
  { whenOk :: String,
    whenFail :: String,
    sayIssue :: ExecutionResult w e (a, NonEmpty Value) -> String, 
    pCheck :: ExecutionResult w e (a, NonEmpty Value) -> Bool 
    -- TODO: maybe put `Assertion` instead of Bool check here ^ and let Tasty do the stuff
  }

not :: Predicate w e a -> Predicate w e a
not predicate =
  let (Predicate wOk wFail ti c) = predicate
   in Predicate wFail wOk ti (Prelude.not . c)

isSuccessOutcome = isRight . outcome -- TODO: make more general assert

shouldSucceed :: Predicate w e a
shouldSucceed =
  Predicate
    "Contract should succeed"
    "Contract should fail"
    (const "But it didn't")
    isSuccessOutcome

shouldFail :: Predicate w e a
shouldFail = Test.Plutip.Predicate.not shouldSucceed

-- can be expressed via `yieldSatisfies`, but gives more debug info this way
shouldYield :: (Show a, Eq a) => a -> Predicate w e a
shouldYield expected =
  (yieldSatisfies (== expected))
    { whenOk = "Should yield '" <> show expected <> "'",
      whenFail = "Should NOT yield '" <> show expected <> "'"
    }

yieldSatisfies :: (Show a) => (a -> Bool) -> Predicate w e a
yieldSatisfies p =
  Predicate
    "Predicate should hold" -- TODO: some optional tag maybe to distinguish cases?
    "Predicate should NOT hold"
    sayIssue'
    checkOutcome
  where
    sayIssue' r = case outcome r of
      Left _ -> "Contract failed"
      Right (a, _) -> "Got: " <> show a

    checkOutcome r =
      case outcome r of
        Left _ -> False
        Right (a, _) -> p a

shouldThrow :: (Show e, Eq e) => e -> Predicate w e a
shouldThrow expected = errorSatisfies (== expected)

errorSatisfies :: Show e => (e -> Bool) -> Predicate w e a
errorSatisfies predicate =
  failReasonSatisfies $ \case
      ContractExecutionError e -> predicate e
      _                        -> False
-- "Error predicate should NOT hold"

failReasonSatisfies :: Show e => (FailureReason e -> Bool) -> Predicate w e a
failReasonSatisfies p =
  Predicate
    "Error predicate should hold"
    "Error should NOT hold"
    sayIssue'
    checkOutcome
  where
    sayIssue' r = case outcome r of
      Left e -> "Got error: " <> show e
      Right _ -> "Contract didn't fail"
    checkOutcome r =
      case outcome r of
        Left e -> p e
        Right _ -> False


