# Revision history for `plutip`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

### Added

- `Plutip` configuration
  - Ability to set custom file where relay node log can be saved after tests run
  - Ability to set to set custom port for `chain-idex`
  - Ability to set custom directory for cluster data files
  - Ability to set budget multiplier on estimated budget for rare case of incorrect estimation
- Default cluster data files delivered via `data-files` package property
- Contract execution result returns current contract state for failure cases too
- Implementing tasty integration
  - Group together contract executions inside a common cluster using `withCluster`
  - Construct test cases from initial wallet distribution, contracts and assertions with `assertExecution`
  - Display contract execution logs with `assertExecutionWith` with tracing options
  - Run multiple contracts in sequence using `withContract` and `withContractAs`
  - Assertions for testing contract execution result (success and failures) and contract state:
    - `shouldSucceed`
    - `shouldFail`
    - `shouldYield`
    - `yieldSatisfies`
    - `stateIs`
    - `stateSatisfies`
    - `shouldThrow`
    - `errorSatisfies`
    - `failReasonSatisfies`
  - Assertions for testing contract execution budget
    - `overallBudgetFits`
    - `assertOverallBudget`
    - `budgetsFitUnder`
  - Combining arbitrary assertions together
  - Building arbitrary assertions with `Predicate`  
  - Initialising wallets and asserting them after contract execution:
    - `initAda`
    - `initLovelace`
    - `initAndAssertAda`
    - `initAndAssertAdaWith`
    - `initAndAssertLovelace`
    - `initAndAssertLovelaceWith`
    - `initAdaAssertValue`
    - `initAdaAssertValueWith`
    - `initLovelaceAssertValue`
    - `initLovelaceAssertValueWith`

## 0.1 -- 2022-02-14

- First MVP release.
