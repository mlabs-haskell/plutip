{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Plutip.Internal.BotPlutusInterface.Run (runContract, runContract_) where

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
import Cardano.Api.Shelley (ProtocolParameters (ProtocolParameters))
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, catchAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Aeson (ToJSON, eitherDecodeFileStrict')
import Data.Default (def)
import Data.Either.Combinators (fromRight)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Plutus.Contract (Contract)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BIS
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, ledgerPkh)
import Test.Plutip.Internal.LocalCluster.Types (ClusterEnv (chainIndexUrl, networkId), FailReason (CaughtException, ContractExecutionError, OtherErr), Outcome (Fail, Success))
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
  m (Outcome w e a)
runContract cEnv bpiWallet contract = do
  pparams <-
    fromRight (error "Could not read protocol parameters file.")
      <$> liftIO (eitherDecodeFileStrict' (BIS.pParamsFile cEnv))

  runContract' pparams `catchAll` (pure . Fail . CaughtException)
  where
    runContract' :: ProtocolParameters -> m (Outcome w e a)
    runContract' pparams = do
      contractInstanceID <- liftIO $ ContractInstanceId <$> UUID.nextRandom
      contractState <- liftIO $ newTVarIO (ContractState Active (mempty :: w))
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
        Left e -> pure $ Fail (ContractExecutionError e)
        Right a -> Success a <$> liftIO (readTVarIO contractState)
