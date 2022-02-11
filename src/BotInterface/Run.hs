{-# LANGUAGE AllowAmbiguousTypes #-}

module BotInterface.Run (runContractTagged, runContract, runContract_) where

import BotInterface.Setup qualified as BIS
import BotInterface.Wallet (BpiWallet, ledgerPkh)
import BotPlutusInterface.Contract qualified as BIC
import BotPlutusInterface.Types (
  CLILocation (Local),
  ContractEnvironment (ContractEnvironment),
  ContractState (ContractState),
  LogLevel (Info),
  PABConfig (
    PABConfig,
    pcChainIndexUrl,
    pcCliLocation,
    pcDryRun,
    pcLogLevel,
    pcNetwork,
    pcOwnPubKeyHash,
    pcPort,
    pcProtocolParams,
    pcProtocolParamsFile,
    pcScriptFileDir,
    pcSigningKeyFileDir,
    pcTxFileDir
  ),
  ceContractInstanceId,
  ceContractState,
  cePABConfig,
  ceWallet,
 )
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, handleAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text (Text, pack)
import Data.UUID.V4 qualified as UUID
import LocalCluster.Types (ClusterEnv (chainIndexUrl, networkId), FailReason (CaughtException, ContractExecutionError, OtherErr), Outcome (Fail, Success), RunResult (RunResult))
import Plutus.Contract (Contract)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Wallet.Types (ContractInstanceId (ContractInstanceId))

runContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  BpiWallet ->
  Contract w s e a ->
  ReaderT ClusterEnv m (RunResult w e a)
runContract = runContractTagged' Nothing

runContract_ ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  BpiWallet ->
  Contract w s e a ->
  ReaderT ClusterEnv m ()
runContract_ bpiWallet contract = void $ runContract bpiWallet contract

runContractTagged ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  Text ->
  BpiWallet ->
  Contract w s e a ->
  ReaderT ClusterEnv m (RunResult w e a)
runContractTagged = runContractTagged' . Just

runContractTagged' ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  Maybe Text ->
  BpiWallet ->
  Contract w s e a ->
  ReaderT ClusterEnv m (RunResult w e a)
runContractTagged' contractTag bpiWallet contract =
  ask
    >>= readProtocolParams
    >>= either
      (return . taggedFail . OtherErr . pack)
      (handleAll handleErr . runContract')
  where
    taggedRes = RunResult contractTag
    taggedFail = taggedRes . Fail
    playGroundWallet = undefined -- ? fixme: seems like not being used by bot interface?
    readProtocolParams = liftIO . eitherDecodeFileStrict' . BIS.pParamsFile
    handleErr = return . taggedFail . CaughtException
    runContract' :: ProtocolParameters -> ReaderT ClusterEnv m (RunResult w e a)
    runContract' pparams = do
      cEnv <- ask
      contractInstanceID <- liftIO $ ContractInstanceId <$> UUID.nextRandom
      contractState <- liftIO $ newTVarIO (ContractState Active (mempty :: w))
      let pabConf =
            PABConfig
              { pcCliLocation = Local
              , pcChainIndexUrl = chainIndexUrl cEnv
              , pcNetwork = networkId cEnv
              , pcProtocolParams = pparams
              , pcScriptFileDir = pack $ BIS.scriptsDir cEnv
              , pcSigningKeyFileDir = pack $ BIS.keysDir cEnv
              , pcTxFileDir = pack $ BIS.txsDir cEnv
              , pcDryRun = False
              , pcProtocolParamsFile = pack $ BIS.pParamsFile cEnv
              , pcLogLevel = Info
              , pcOwnPubKeyHash = ledgerPkh bpiWallet
              , pcPort = 9080
              }
          contractEnv =
            ContractEnvironment
              { cePABConfig = pabConf
              , ceContractState = contractState
              , ceWallet = playGroundWallet
              , ceContractInstanceId = contractInstanceID
              }
      res <- liftIO $ BIC.runContract contractEnv playGroundWallet contract
      case res of
        Left e -> return $ taggedFail (ContractExecutionError e)
        Right a -> taggedRes . Success a <$> liftIO (readTVarIO contractState)
