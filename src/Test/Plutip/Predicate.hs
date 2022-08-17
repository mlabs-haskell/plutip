{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides some predicates or assertions, that could be used together with
--  `Test.Plutip.Contract.assertExecution` to run tests for Contract in private testnet.
--
--  Module also exports `Predicate` constructor itself, so any arbitrary predicate could be made.
module Test.Plutip.Predicate (
  Predicate (..),
  pTag,
  shouldSucceed,
  shouldHave,
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
  assertOverallBudget,
  overallBudgetFits,
  noBudgetsMessage,
) where

import BotPlutusInterface.Types (TxBudget (TxBudget), mintBudgets, spendBudgets)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), TxId, Value)
import PlutusCore.Evaluation.Machine.ExMemory (CostingInteger)
import Prettyprinter (Doc, align, defaultLayoutOptions, indent, layoutPretty, viaShow, vsep, (<+>))
import Prettyprinter.Render.String (renderString)
import Test.Plutip.Internal.Types (
  ExecutionResult (ExecutionResult, contractState, outcome),
  FailureReason (CaughtException, ContractExecutionError),
  budgets,
  isSuccessful,
 )
import Test.Plutip.Contract.Types (Predicate(..), NthWallet(..), Wallets)
import Test.Plutip.Tools.Format (fmtExBudget, fmtTxBudgets)
import Text.Show.Pretty (ppShow)

shouldHave :: forall idx w e a idxs. (NthWallet idx idxs, Show w, Show e, Show a)
           => Value
           -> Predicate w e a idxs
shouldHave value =
  Predicate
    ("should have value " ++ show value)
    ("Error: Expected value " ++ show value)
    (mappend "But it didn't.\nResult: " . renderString . layoutPretty defaultLayoutOptions . prettyExecutionResult)
    (either (const False) (haveVal @idx value . snd) . outcome)


haveVal :: forall idx idxs. (NthWallet idx idxs)
           => Value
           -> Wallets idxs Value
           -> Bool
haveVal value ws = value == nthWallet @idx ws



-- | `positive` description of `Predicate` that will be used as test case tag.
--
-- @since 0.2
pTag :: Predicate w e a idxs -> String
pTag = positive

-- | Switch the meaning of `Predicate` to the opposite.
--
-- @since 0.2
not :: Predicate w e a idxs -> Predicate w e a idxs
not predicate =
  let (Predicate pos' neg' dbgInfo' check') = predicate
   in Predicate neg' pos' dbgInfo' (Prelude.not . check')

-- Predefined predicates --

-- Basic success/fail --

-- | Check that Contract didn't fail.
--
-- @since 0.2
shouldSucceed :: (Show e, Show a, Show w) => Predicate w e a idxs
shouldSucceed =
  Predicate
    "Contract should succeed"
    "Contract should fail"
    (mappend "But it didn't.\nResult: " . renderString . layoutPretty defaultLayoutOptions . prettyExecutionResult)
    isSuccessful

-- | Pretty print ExecutionResult hiding budget stats and logs.
prettyExecutionResult :: (Show e, Show w, Show a) => ExecutionResult e w a -> Doc ann
prettyExecutionResult ExecutionResult {outcome, contractState} =
  vsep
    [ "Execution result {"
    , indent 4 $
        align $
          vsep
            [ "outcome:" <+> viaShow outcome
            , "final state: " <+> viaShow contractState
            ]
    , "}"
    ]

-- | Check that Contract didn't succeed.
--
-- @since 0.2
shouldFail :: (Show e, Show a, Show w) => Predicate w e a idxs
shouldFail = Test.Plutip.Predicate.not shouldSucceed

-- Contract result --

-- | Check that Contract returned the expected value.
--
-- @since 0.2
shouldYield :: (Show a, Eq a) => a -> Predicate w e a idxs
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
yieldSatisfies :: (Show a) => String -> (a -> Bool) -> Predicate w e a idxs
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
stateIs :: (Show w, Eq w) => w -> Predicate w e a idxs
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
stateSatisfies :: Show w => String -> (w -> Bool) -> Predicate w e a idxs
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
shouldThrow :: (Show e, Eq e) => e -> Predicate w e a idxs
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
errorSatisfies :: Show e => String -> (e -> Bool) -> Predicate w e a idxs
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
failReasonSatisfies :: Show e => String -> (FailureReason e -> Bool) -> Predicate w e a idxs
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

-- | Check if overall budget (sum of all CPU and MEM budgets) less than or equal
--  to specified limits.
--
-- If heck fails, all collected budgets will be printed to output.
--
-- @since 0.2
overallBudgetFits :: ExCPU -> ExMemory -> Predicate w e a idxs
overallBudgetFits cpuLimit memLimit =
  let p =
        assertOverallBudget
          ("Budget should fit " ++ fmtExBudget (ExBudget cpuLimit memLimit))
          (<= cpuLimit)
          (<= memLimit)
   in p {negative = "Budget should NOT fit " ++ fmtExBudget (ExBudget cpuLimit memLimit)}

-- | Check if overall cpu and mem budgets satisfy their predicates.
-- (more general version of `overallBudgetFits`)
--
-- If heck fails, all collected budgets will be printed to output.
--
-- @since 0.2
assertOverallBudget :: String -> (ExCPU -> Bool) -> (ExMemory -> Bool) -> Predicate w e a idxs
assertOverallBudget description cpuCheck memCheck =
  let positive = description
      negative = ("Should violate '" <> description <> "'")
      debugInfo (budgets -> bs)
        | null bs =
          -- at least some budgets expected for assertion
          noBudgetsMessage
        | otherwise =
          let budget = foldMap sumBudgets bs
           in mconcat
                [ "Overall budget: "
                , fmtExBudget budget
                , "\nBudget details:\n"
                , fmtTxBudgets bs
                ]

      pCheck (budgets -> bs)
        -- at least some budgets expected for assertion
        | null bs = False
        | otherwise =
          let ExBudget cpu mem = foldMap sumBudgets bs
           in cpuCheck cpu && memCheck mem

      sumBudgets :: TxBudget -> ExBudget
      sumBudgets (TxBudget spend mint) =
        mconcat $ Map.elems spend ++ Map.elems mint
   in Predicate {..}

-- | More precise fitting assertion: it checks each script
-- and minting policy budget if they fit specified limits. Limits are specified
-- separately for scripts and minting policies.

-- If check fails, any budget that didn't fit limit, will be printed to output.
--
-- @since 0.2
budgetsFitUnder :: Limit 'Script -> Limit 'Policy -> Predicate w e a idxs
budgetsFitUnder (Limit sCpu sMem) (Limit pCpu pMem) =
  let positive =
        mconcat
          [ "Each script fits "
          , fmtExBudget (ExBudget sCpu sMem)
          , " and each policy fits "
          , fmtExBudget (ExBudget pCpu pMem)
          ]
      negative = "TBD negative"
      debugInfo (budgets -> bs)
        | null bs =
          -- at least some budgets expected for assertion
          noBudgetsMessage
        | filtered <- processMap bs
          , Prelude.not (null filtered) =
          "Budgets that didn't fit the limit :\n" ++ fmtTxBudgets filtered
        | otherwise =
          -- show at least some debug info
          "Collected budgets:\n" ++ fmtTxBudgets bs

      pCheck (budgets -> bs) = null (processMap bs)
      -- TODO: second iteration over stats happens here,
      -- maybe `pCheck` and `debugInfo` could be somehow combined to avoid this

      -- process a map with budgets collecting only those that do not fit into the specified limit
      processMap :: Map TxId TxBudget -> Map TxId TxBudget
      processMap =
        Map.foldMapWithKey
          ( \txId bdg ->
              let badOnes = findNonFitting bdg
               in if isEmpty badOnes
                    then -- if transaction doesn't have "bad" budgets,
                    -- it won't be included into debug info
                      mempty
                    else Map.singleton txId badOnes
          )

      -- "filter" `TxBudget` by removing only script or minting budgets
      -- that fit limits
      findNonFitting b =
        filterBudget (Prelude.not . fits sCpu sMem) (Prelude.not . fits pCpu pMem) b

      fits cpuLimit memLimit (ExBudget cpu' mem') =
        cpu' <= cpuLimit && mem' <= memLimit
   in Predicate {..}

noBudgetsMessage :: String
noBudgetsMessage = "Empty budgets data (probably, no scripts or policies in contract or contract failed)"

-- to protect from accidental `scriptLimit` <-> `policyLimit` swapping
-- while making `budgetsFitUnder` predicate
data LimitType = Script | Policy
data Limit (a :: LimitType) = Limit ExCPU ExMemory
  deriving stock (Show)

-- | Construct script limit for `budgetsFitUnder`
scriptLimit :: CostingInteger -> CostingInteger -> Limit a
scriptLimit cpu mem =
  Limit (ExCPU cpu) (ExMemory mem)

-- | Construct policy limit for `budgetsFitUnder`
policyLimit :: CostingInteger -> CostingInteger -> Limit a
policyLimit cpu mem =
  Limit (ExCPU cpu) (ExMemory mem)

-- | Modifies ("filters") `TxBudget` in such a way, that it contains only
-- script and minting policy budgets that satisfy predicates.
filterBudget :: (ExBudget -> Bool) -> (ExBudget -> Bool) -> TxBudget -> TxBudget
filterBudget spendFilter mintFilter txB =
  let newSb = Map.filter spendFilter (spendBudgets txB)
      newMb = Map.filter mintFilter (mintBudgets txB)
   in TxBudget newSb newMb

isEmpty :: TxBudget -> Bool
isEmpty (TxBudget s p) = null s && null p

