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
    pcTxFileDir
  ),
  ceContractInstanceId,
  ceContractState,
  cePABConfig,
 )
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except.Extra (
  firstExceptT,
  handleExceptT,
  hoistEither,
  runExceptT,
 )
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
  ExecutionResult (ExecutionResult, contractState, outcome),
  FailureReason (CaughtException, ContractExecutionError),
 )
import Wallet.Types (ContractInstanceId (ContractInstanceId))

runContract_ ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m ()
runContract_ e w c = void $ runContract e w c

runContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m (ExecutionResult w e a)
runContract cEnv bpiWallet contract = do
  pparams <-
    fromRight (error "Could not read protocol parameters file.")
      <$> liftIO (eitherDecodeFileStrict' (BIS.pParamsFile cEnv))
  contractState <- liftIO $ newTVarIO (ContractState Active (mempty :: w))
  result <-
    runExceptT $
      runContract' pparams contractState
        >>= firstExceptT ContractExecutionError . hoistEither
  currentState <- liftIO (readTVarIO contractState)
  return $
    ExecutionResult
      { outcome = result
      , contractState = csObservableState currentState
      }
  where
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
              , pcOwnPubKeyHash = walletPkh bpiWallet
              , pcPort = 9080
              , pcEnableTxEndpoint = False
              }
          contractEnv =
            ContractEnvironment
              { cePABConfig = pabConf
              , ceContractState = contractState
              , ceContractInstanceId = contractInstanceID
              }
      handleExceptT
        (\(e :: SomeException) -> CaughtException e)
        (liftIO $ BIC.runContract contractEnv contract)
