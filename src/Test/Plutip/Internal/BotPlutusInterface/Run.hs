{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

module Test.Plutip.Internal.BotPlutusInterface.Run (
  defCollateralSize,
  runContractWithLogLvl,
  runContract,
  runContract_,
) where

import BotPlutusInterface.Contract qualified as BIC
import BotPlutusInterface.Types (
  CLILocation (Local),
  ContractEnvironment (ContractEnvironment),
  ContractState (ContractState, csObservableState),
  LogLevel (Error),
  LogType (AnyLog),
  PABConfig (
    PABConfig,
    pcBudgetMultiplier,
    pcChainIndexUrl,
    pcCliLocation,
    pcDryRun,
    pcEnableTxEndpoint,
    pcLogLevel,
    pcNetwork,
    pcOwnPubKeyHash,
    pcPort,
    pcProtocolParams,
    pcProtocolParamsFile,
    pcScriptFileDir,
    pcSigningKeyFileDir,
    pcTipPollingInterval,
    pcTxFileDir
  ),
  TxStatusPolling (TxStatusPolling),
  ceContractLogs,
  ceContractState,
  ceContractStats,
  pcCollectLogs,
  pcCollectStats,
  pcMetadataDir,
  pcOwnStakePubKeyHash,
  pcTxStatusPolling,
 )
import BotPlutusInterface.Types qualified as Bpi
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Catch (SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Either.Combinators (fromRight)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Ledger (unPaymentPubKeyHash)
import Plutus.Contract (Contract)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Test.Plutip.Config (PlutipConfig (budgetMultiplier))
import Test.Plutip.Internal.BotPlutusInterface.Lookups (makeWalletInfo)
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BIS
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiWallet, ownPaymentPubKeyHash, ownStakePubKeyHash)
import Test.Plutip.Internal.Types (
  ClusterEnv (chainIndexUrl, networkId, plutipConf),
  ExecutionResult (ExecutionResult),
  FailureReason (CaughtException, ContractExecutionError),
 )
import Wallet.Types (ContractInstanceId (ContractInstanceId))

-- | default collateral size that's to be used as collateral.
defCollateralSize :: Integer
defCollateralSize = 10_000_000

runContract_ ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m ()
runContract_ e w c = void $ runContract e w c

runContract ::
  (ToJSON w, Monoid w, MonadIO m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m (ExecutionResult w e a)
runContract = runContractWithLogLvl $ Error [AnyLog]

-- | "By default" contracts a being run with `runContract` with maximum severity
-- and logs are collected by framework and can be obtained from `ExecutionResult`,
-- but when working with Plutip in interactive mode it is often useful to
-- observe (or demonstrate) live logs. This function provides possibility to
-- obtain this functionality and see live logs by setting desired log level.
runContractWithLogLvl ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m) =>
  Bpi.LogLevel ->
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m (ExecutionResult w e a)
runContractWithLogLvl logLvl cEnv bpiWallet contract = do
  pparams <-
    fromRight (error "Could not read protocol parameters file.")
      <$> liftIO (eitherDecodeFileStrict' (BIS.pParamsFile cEnv))

  contactEnv <- liftIO $ mkEnv (mkPabConfig pparams)

  runContract' contactEnv
  where
    mkEnv pabConf =
      ContractEnvironment pabConf
        <$> ContractInstanceId
        <$> UUID.nextRandom
        <*> newTVarIO (ContractState Active (mempty :: w))
        <*> newTVarIO mempty
        <*> newTVarIO mempty
        <*> (Bpi.CollateralVar <$> newTVarIO Nothing)

    mkPabConfig pparams =
      PABConfig
        { pcCliLocation = Local
        , pcChainIndexUrl = chainIndexUrl cEnv
        , pcNetwork = networkId cEnv
        , pcProtocolParams = pparams
        , pcScriptFileDir = Text.pack $ BIS.scriptsDir cEnv
        , pcSigningKeyFileDir = Text.pack $ BIS.keysDir cEnv
        , pcTxFileDir = Text.pack $ BIS.txsDir cEnv
        , pcDryRun = False
        , pcProtocolParamsFile = Text.pack $ BIS.pParamsFile cEnv
        , pcLogLevel = logLvl
        , pcOwnPubKeyHash = unPaymentPubKeyHash $ ownPaymentPubKeyHash walletInfo
        , pcOwnStakePubKeyHash = ownStakePubKeyHash walletInfo
        , pcTipPollingInterval = 1_000_000
        , pcPort = 9080
        , pcEnableTxEndpoint = False
        , pcMetadataDir = Text.pack $ BIS.metadataDir cEnv
        , pcCollectStats = True
        , pcCollectLogs = True
        , pcBudgetMultiplier = budgetMultiplier (plutipConf cEnv)
        , pcTxStatusPolling = TxStatusPolling 500_000 8
        , pcCollateralSize = fromInteger defCollateralSize
        }

    walletInfo = makeWalletInfo bpiWallet

    runContract' :: ContractEnvironment w -> m (ExecutionResult w e a)
    runContract' contractEnv = do
      res <- liftIO $ try @SomeException (BIC.runContract contractEnv contract)

      let partialResult = case res of
            Left e ->
              ExecutionResult (Left $ CaughtException e)
            Right (Left e) ->
              ExecutionResult (Left $ ContractExecutionError e)
            Right (Right a) ->
              ExecutionResult (Right a)

      endState <- liftIO (readTVarIO $ ceContractState contractEnv)
      stats <- liftIO (readTVarIO $ ceContractStats contractEnv)
      logs <-
        -- `bpi` collects logs in reversed order adding entries to the head of the `List`
        -- we need them user-friendly-reversed
        Bpi.LogsList . reverse . Bpi.getLogsList
          <$> liftIO (readTVarIO $ ceContractLogs contractEnv)
      return $ partialResult stats (csObservableState endState) logs
