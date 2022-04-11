{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Plutip.Internal.BotPlutusInterface.Run (runContract, runContract_) where

import BotPlutusInterface.Contract qualified as BIC
import BotPlutusInterface.Types (
  CLILocation (Local),
  ContractEnvironment (ContractEnvironment),
  ContractState (ContractState, csObservableState),
  LogLevel (Info),
  PABConfig (
    PABConfig,
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
    pcSlotConfig,
    pcTipPollingInterval,
    pcTxFileDir
  ),
  ceContractInstanceId,
  ceContractState,
  cePABConfig,
  pcOwnStakePubKeyHash,
 )
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Catch (SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Default (def)
import Data.Either.Combinators (fromRight)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Plutus.Contract (Contract)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BIS
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet (walletPkh))
import Test.Plutip.Internal.Types (
  ClusterEnv (chainIndexUrl, networkId),
  ExecutionResult (ExecutionResult),
  FailureReason (CaughtException, ContractExecutionError),
 )
import Wallet.Types (ContractInstanceId (ContractInstanceId))

runContract_ ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m ()
runContract_ e w c = void $ runContract e w c

runContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m (ExecutionResult w e a)
runContract cEnv bpiWallet contract = do
  pparams <-
    fromRight (error "Could not read protocol parameters file.")
      <$> liftIO (eitherDecodeFileStrict' (BIS.pParamsFile cEnv))
  contractState <- liftIO $ newTVarIO (ContractState Active (mempty :: w))
  runContract' pparams contractState
  where
    runContract' :: ProtocolParameters -> TVar (ContractState w) -> m (ExecutionResult w e a)
    runContract' pparams contractState = do
      contractInstanceID <- liftIO $ ContractInstanceId <$> UUID.nextRandom
      let pabConf =
            PABConfig
              { pcCliLocation = Local
              , pcChainIndexUrl = chainIndexUrl cEnv
              , pcNetwork = networkId cEnv
              , pcProtocolParams = pparams
              , pcSlotConfig = def
              , pcScriptFileDir = Text.pack $ BIS.scriptsDir cEnv
              , pcSigningKeyFileDir = Text.pack $ BIS.keysDir cEnv
              , pcTxFileDir = Text.pack $ BIS.txsDir cEnv
              , pcDryRun = False
              , pcProtocolParamsFile = Text.pack $ BIS.pParamsFile cEnv
              , pcLogLevel = Info
              , -- , pcForceBudget = bpiForceBudget cEnv
                pcOwnPubKeyHash = walletPkh bpiWallet
              , pcOwnStakePubKeyHash = Nothing
              , pcTipPollingInterval = 1_000_000
              , pcPort = 9080
              , pcEnableTxEndpoint = False
              }
          contractEnv =
            ContractEnvironment
              { cePABConfig = pabConf
              , ceContractState = contractState
              , ceContractInstanceId = contractInstanceID
              }

      res <- liftIO $ try @SomeException (BIC.runContract' contractEnv contract)

      let resultWoState = case res of
            Left e ->
              ExecutionResult (Left $ CaughtException e) Nothing
            Right (Left e, budgets) ->
              ExecutionResult (Left $ ContractExecutionError e) (Just budgets)
            Right (Right a, budgets) ->
              ExecutionResult (Right a) (Just budgets)
      endState <- liftIO (readTVarIO contractState)
      return $ resultWoState (csObservableState endState)
