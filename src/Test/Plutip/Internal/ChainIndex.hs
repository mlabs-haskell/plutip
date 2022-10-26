module Test.Plutip.Internal.ChainIndex (
  handleChainIndexLaunch,
) where

import Cardano.Api qualified as CAPI
import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Data.Severity qualified as Severity
import Cardano.Launcher.Node (nodeSocketFile)

import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Retry (constantDelay, limitRetries, recoverAll)
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config qualified as ChainIndex
import Plutus.ChainIndex.Logging (defaultConfig)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http), mkClientEnv, runClientM)
import System.FilePath ((</>))
import Test.Plutip.Config (
  ChainIndexMode (CustomPort, DefaultPort, NotNeeded),
 )
import Test.Plutip.Internal.Types (
  RunningNode (RunningNode),
 )
import UnliftIO.Exception (throwString)

import Cardano.Wallet.Primitive.Types (
  NetworkParameters (NetworkParameters),
  SlotLength (SlotLength),
  SlottingParameters (SlottingParameters),
 )
import Data.Default (Default (def))
import Data.Function ((&))
import Data.Time (nominalDiffTimeToSeconds)
import Ledger (Slot (Slot))
import Ledger.TimeSlot (SlotConfig (scSlotLength))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Plutus.ChainIndex (Tip (Tip))
import Plutus.ChainIndex.Client qualified as ChainIndexClient
import Plutus.ChainIndex.Config qualified as CIC
import PlutusPrelude ((.~), (^.))

type ChainIndexPort = Int

-- | Handle launch of `chain-index`. Depending on `ChainIndexMode`, it can be
-- launched on default port, on custom port or not launched at all.
-- In the latter case no port will be returned.
handleChainIndexLaunch ::
  ChainIndexMode ->
  RunningNode ->
  FilePath ->
  IO (Maybe ChainIndexPort)
handleChainIndexLaunch mode rn dir = do
  maybePort <-
    case mode of
      DefaultPort -> do
        Just <$> launchChainIndex (CIC.cicPort ChainIndex.defaultConfig) rn dir
      CustomPort port' -> do
        Just <$> launchChainIndex (fromEnum port') rn dir
      NotNeeded -> pure Nothing
  reportLaunch maybePort
  pure maybePort
  where
    reportLaunch = \case
      Just p -> putStrLn $ "Chain index started at port " <> show p
      _ -> pure ()

-- | Launch the chain index in a separate thread.
launchChainIndex :: Int -> RunningNode -> FilePath -> IO Int
launchChainIndex port (RunningNode sp _block0 (netParams, _vData) _) dir = do
  let (NetworkParameters _ (SlottingParameters (SlotLength slotLen) _ _ _) _) = netParams

  config <- defaultConfig
  CM.setMinSeverity config Severity.Notice
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        CIC.defaultConfig
          & CIC.socketPath .~ nodeSocketFile sp
          & CIC.dbPath .~ dbPath
          & CIC.networkId .~ CAPI.Mainnet
          & CIC.port .~ port
          & CIC.slotConfig .~ (def {scSlotLength = toMilliseconds slotLen})

  void $ async $ void $ ChainIndex.runMainWithLog (const $ return ()) config chainIndexConfig
  waitForChainIndex
  return $ chainIndexConfig ^. CIC.port
  where
    toMilliseconds = floor . (1e3 *) . nominalDiffTimeToSeconds

    waitForChainIndex = do
      -- TODO: move this to config; ideally, separate chain-index launch from cluster launch
      let policy = constantDelay 1_000_000 <> limitRetries 60
      recoverAll policy $ \_ -> do
        tip <- queryTipWithChIndex
        case tip of
          Right (Tip (Slot _) _ _) -> pure ()
          a ->
            throwString $
              "Timeout waiting for chain-index to start indexing. Last response:\n"
                <> either show show a

    queryTipWithChIndex = do
      manager' <- newManager defaultManagerSettings
      runClientM ChainIndexClient.getTip $ mkClientEnv manager' (BaseUrl Http "localhost" port "")
