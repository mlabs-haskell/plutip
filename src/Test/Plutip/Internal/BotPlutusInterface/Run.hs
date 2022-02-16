{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Plutip.Internal.BotPlutusInterface.Run (runContractTagged, runContract, runContract_) where

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
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, handleAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Default (def)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text (Text, pack)
import Data.UUID.V4 qualified as UUID
import Plutus.Contract (Contract)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BIS
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, ledgerPkh)
import Test.Plutip.Internal.LocalCluster.Types (ClusterEnv (chainIndexUrl, networkId), FailReason (CaughtException, ContractExecutionError, OtherErr), Outcome (Fail, Success), RunResult (RunResult))
import Wallet.Types (ContractInstanceId (ContractInstanceId))

-- | Run contract on private network
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

-- | Run contract on private network propagating arbitrary description to `RunResult`
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
              , pcSlotConfig = def
              , pcScriptFileDir = pack $ BIS.scriptsDir cEnv
              , pcSigningKeyFileDir = pack $ BIS.keysDir cEnv
              , pcTxFileDir = pack $ BIS.txsDir cEnv
              , pcDryRun = False
              , pcProtocolParamsFile = pack $ BIS.pParamsFile cEnv
              , pcLogLevel = Info
              , pcOwnPubKeyHash = ledgerPkh bpiWallet
              , pcPort = 9080
              , pcEnableTxEndpoint = False
              }
          contractEnv =
            ContractEnvironment
              { cePABConfig = pabConf
              , ceContractState = contractState
              , ceContractInstanceId = contractInstanceID
              }
      res <- liftIO $ BIC.runContract contractEnv contract
      case res of
        Left e -> return $ taggedFail (ContractExecutionError e)
        Right a -> taggedRes . Success a <$> liftIO (readTVarIO contractState)
