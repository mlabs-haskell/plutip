# Revision history for `plutip`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## Unreleased

### Added

- Implementing tasty integration
  - Group together contract executions inside a common cluster using `withCluster`
  - Assertions for testing contract success and failure, and for several outcomes:
    - `shouldSucceed`
    - `shouldFail`
    - `shouldYield`
    - `shouldHaveObservableState`
    - `assertYieldedResultWith`
    - `assertObservableStateWith`
  - Initialising wallets and asserting them after contract execution:
    - `initAda`
    - `initLovelace`
    - `initAndAssertAda`
    - `initAndAssertLovelace`
    - `initAdaAssertValue`
    - `initLovelaceAssertValue`

## 0.1 -- 2022-02-14

- First MVP release.
