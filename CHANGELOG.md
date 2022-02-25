# Revision history for `plutip`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

### Added

- `Plutip` configuration
  - Ability to set custom file where relay node log can be saved after tests run
  - Ability to set to set custom port for `chain-idex`
  - Ability to set custom directory for cluster data files
- Default cluster data files delivered via `data-files` package property
- Contract execution result returns current contract state for failure cases too
- Implementing tasty integration
  - Group together contract executions inside a common cluster using `withCluster`
  - Assertions for testing contract success and failure, and for several outcomes:
    - `shouldSucceed`
    - `shouldFail`
    - `shouldYield`
    - `shouldHaveObservableState`
    - `assertYieldedResultWith`
    - `assertObservableStateWith`
    - `assertFailure`
    - `assertContractError`
    - `shouldThrowContractError`
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
