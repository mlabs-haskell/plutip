{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
  budgetsFitUnder,
  policyLimit,
  scriptLimit,
) where

import BotPlutusInterface.Types (TxBudget (TxBudget), mintBudgets, spendBudgets)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), TxId, Value)
import PlutusCore.Evaluation.Machine.ExMemory (CostingInteger)
import Test.Plutip.Internal.Types (
  ExecutionResult (contractState, outcome),
  FailureReason (CaughtException, ContractExecutionError),
  budgets,
  isSuccessful,
 )
import Test.Plutip.Tools.Format (formatTxBudgets)
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

-- | Check if:
--
-- @since 0.2
budgetsFitUnder :: Limit 'Script -> Limit 'Policy -> Predicate w e a
budgetsFitUnder (Limit sCpu sMem) (Limit pCpu pMem) =
  let positive = "Each validator and policy fits limits"
      negative = "TBD negative"
      debugInfo er =
        case budgets er of
          Nothing ->
            -- case when exception happened during contract run and no result returned
            "No budgets available "
          Just bs
            | null bs ->
              -- we expect at least some budgets
              "Empty budgets map (no scripts or policies in contract?)"
            | filtered <- processMap bs
              , Prelude.not (null filtered) ->
              "Budgets that didn't fit:\n" ++ formatTxBudgets filtered
            | otherwise -> ""

      -- TDOD: refactor; some tests won't hurt
      processMap :: Map TxId TxBudget -> Map TxId TxBudget
      processMap =
        Map.foldMapWithKey
          ( \txId bdg ->
              let overf = getOverf bdg
               in if isEmpty overf
                    then mempty
                    else Map.singleton txId overf
          )
      pCheck er =
        case budgets er of
          Nothing -> False -- case when exception happened during contract run and no result returned
          Just bsm ->
            null (processMap bsm)
      -- TODO: second iteration over stats happens here,
      -- maybe `pCheck` and `debugInfo` could be somehow combined to avoid this

      getOverf b =
        filterBudget (Prelude.not . fits sCpu sMem) (Prelude.not . fits pCpu pMem) b

      fits cpuLimit memLimit (ExBudget cpu' mem') =
        cpu' <= cpuLimit && mem' <= memLimit
   in Predicate {..}

-- to protect from accidental `scriptLimit` <-> `policyLimit` swapping
-- while making `budgetsFitUnder` predicate
data LimitType = Script | Policy
data Limit (a :: LimitType) = Limit ExCPU ExMemory

scriptLimit :: CostingInteger -> CostingInteger -> Limit a
scriptLimit cpu mem =
  Limit (ExCPU cpu) (ExMemory mem)

policyLimit :: CostingInteger -> CostingInteger -> Limit a
policyLimit cpu mem =
  Limit (ExCPU cpu) (ExMemory mem)

filterBudget :: (ExBudget -> Bool) -> (ExBudget -> Bool) -> TxBudget -> TxBudget
filterBudget spendFilter mintFilter txB =
  let newSb = Map.filter spendFilter (spendBudgets txB)
      newMb = Map.filter mintFilter (mintBudgets txB)
   in TxBudget newSb newMb

-- TODO: remove after bpi update
instance Semigroup TxBudget where
  TxBudget s m <> TxBudget s' m' = TxBudget (s <> s') (m <> m')

instance Monoid TxBudget where
  mempty = TxBudget mempty mempty

isEmpty (TxBudget s p) = null s && null p
