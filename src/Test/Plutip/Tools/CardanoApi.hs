{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Helpers based on Cardano.Api (do not use `cardano-cli` executable)
module Test.Plutip.Tools.CardanoApi (
  currentBlock,
  utxosAtAddress,
  queryProtocolParams,
  queryTip,
  awaitWalletFunded,
  AwaitWalletFundedError (AwaitingCapiError, AwaitingTimeoutError),
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Arrow (right)
import Control.Exception (Exception)
import Control.Retry (constantDelay, limitRetries, retrying)
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Test.Plutip.Internal.Types (ClusterEnv (runningNode))

newtype CardanoApiError
  = SomeError String
  deriving stock (Eq, Show, Generic)

instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
currentBlock :: ClusterEnv -> IO (Either _ (WithOrigin C.BlockNo))
currentBlock (runningNode -> rn) = do
  let query = C.QueryChainBlockNo
      info = connectionInfo rn
  C.queryNodeLocalState info Nothing query

utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.BabbageEra))
utxosAtAddress (runningNode -> rn) addr = do
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query =
      shellyBasedBabbageQuery
        (C.QueryUTxO $ C.QueryUTxOByAddress (Set.singleton addr))

queryProtocolParams :: ClusterEnv -> IO (Either CardanoApiError ProtocolParameters)
queryProtocolParams (runningNode -> rn) =
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query = shellyBasedBabbageQuery C.QueryProtocolParameters

connectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
connectionInfo (RunningNode socket _ _ _) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    C.Mainnet
    (nodeSocketFile socket)

queryTip :: RunningNode -> IO C.ChainTip
queryTip = C.getLocalChainTip . connectionInfo

shellyBasedBabbageQuery ::
  C.QueryInShelleyBasedEra C.BabbageEra result ->
  C.QueryInMode C.CardanoMode (Either EraMismatch result)
shellyBasedBabbageQuery =
  C.QueryInEra C.BabbageEraInCardanoMode
    . C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage

flattenQueryResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either CardanoApiError b
flattenQueryResult = \case
  Right (Right res) -> Right res
  err -> Left $ SomeError (show err)

data AwaitWalletFundedError
  = AwaitingCapiError CardanoApiError
  | AwaitingTimeoutError

instance Show AwaitWalletFundedError where
  show (AwaitingCapiError (SomeError e)) = e
  show AwaitingTimeoutError = "Awaiting funding transaction timed out."

-- | Waits till specified address is funded using cardano-node query.
-- Performs 20 tries with 0.2 seconds between tries, which should be a sane default.
-- Waits till there's any utxos at an address - works for us as funds will be send with tx per address.
awaitWalletFunded ::
  ClusterEnv ->
  C.AddressAny ->
  IO (Either AwaitWalletFundedError ())
awaitWalletFunded cenv addr = toErrorMsg <$> retrying policy checkResponse action
  where
    -- With current defaults the slot length is 0.2s and block gets produced about every second slot.
    -- We are expected to wait 0.4s, waiting 4s we are almost guaranteed (p>0.9999)
    delay = 200_000 -- in microseconds, 0.2s.
    policy = constantDelay delay <> limitRetries 20

    action _ = right (M.null . C.unUTxO) <$> utxosAtAddress cenv addr

    checkResponse _ = return . fromRight False

    toErrorMsg = \case
      Left e -> Left $ AwaitingCapiError e
      Right noUtxos ->
        if noUtxos
          then Left AwaitingTimeoutError
          else Right ()
