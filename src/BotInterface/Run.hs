{-# LANGUAGE AllowAmbiguousTypes #-}

module BotInterface.Run (execute, runWrapped) where

import BotInterface.Setup qualified as BIS
import BotInterface.Wallet (BpiWallet, ledgerPkh)
import BotPlutusInterface.Contract (runContract)
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
import Control.Monad.Catch (MonadCatch, handleAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text (Text, pack)
import Data.UUID.V4 qualified as UUID
import LocalCluster.Types (ClusterEnv (chainIndexUrl, networkId))
import Plutus.Contract (Contract)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Wallet.Types (ContractInstanceId (ContractInstanceId))

data FailReason e
  = ContractErr e
  | OtherErr Text
  deriving stock (Show)

data RunResult w e a
  = RunSuccess
      { contractResult :: a
      , contractState :: ContractState w
      }
  | RunFailed {reason :: FailReason e}
  deriving stock (Show)

execute ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (m :: Type -> Type).
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  BpiWallet ->
  Contract w s e a ->
  ReaderT ClusterEnv m (RunResult w e a)
execute bpiWallet contract =
  ask
    >>= readProtocolParams
    >>= either
      (return . RunFailed . OtherErr . pack)
      (handleAll handleErr . runContract')
  where
    playGroundWallet = undefined -- ? fixme: seems like not being used by bot interface?
    readProtocolParams = liftIO . eitherDecodeFileStrict' . BIS.pParamsFile
    handleErr = return . RunFailed . OtherErr . pack . show
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
              , pcDryRun = True
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
      res <- liftIO $ runContract contractEnv playGroundWallet contract
      case res of
        Left e -> return $ RunFailed (ContractErr e)
        Right a -> RunSuccess a <$> liftIO (readTVarIO contractState)

runWrapped ::
  (ToJSON w, Monoid w, MonadIO m, MonadCatch m) =>
  ClusterEnv ->
  BpiWallet ->
  Contract w s e a ->
  m (RunResult w e a)
runWrapped cEnv bpiWallet contract = runReaderT (execute bpiWallet contract) cEnv
