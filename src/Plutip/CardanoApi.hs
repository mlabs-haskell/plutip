{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Helpers for querying the node based on Cardano.Api (do not use `cardano-cli` executable)
module Plutip.CardanoApi (
  currentBlock,
  utxosAtAddress,
  utxosAtAddresses,
  queryProtocolParams,
  queryTip,
  awaitUtxosNumber,
  awaitUtxos,
  CardanoApiError,
  AwaitWalletFundedError (AwaitingCapiError, AwaitingTimeoutError),
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley (BabbageEra, ProtocolParameters, UTxO)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)

import Control.Arrow (right)
import Control.Exception (Exception)
import Control.Retry (constantDelay, limitRetries, retrying)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (nominalDiffTimeToSeconds)
import GHC.Generics (Generic)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Plutip.Config (PlutipConfig (extraConfig))
import Plutip.Launch.Extra.Types (ExtraConfig (ecSlotLength))
import Plutip.Types (ClusterEnv (plutipConf, runningNode), RunningNode (RunningNode))

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

utxosAtAddresses :: ClusterEnv -> [C.AddressAny] -> IO (Either CardanoApiError (C.UTxO C.BabbageEra))
utxosAtAddresses (runningNode -> rn) addrs = do
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query =
      shellyBasedBabbageQuery
        (C.QueryUTxO $ C.QueryUTxOByAddress $ Set.fromList addrs)

utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.BabbageEra))
utxosAtAddress cenv = utxosAtAddresses cenv . pure

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

-- | Wait till specified addresses are funded,
-- by checking if they collectively own at least the expected number of utxos.
-- Performs 60 cardano-node queries with `retryDelay` seconds between tries.
awaitUtxosNumber ::
  ClusterEnv ->
  [C.AddressAny] ->
  Int ->
  IO (Either AwaitWalletFundedError ())
awaitUtxosNumber cenv addrs utxosNum = awaitUtxos cenv addrs (\x -> utxosNum <= Map.size (C.unUTxO x))

-- | Wait till specified addresses are funded,
-- by checking if the UTXOs satisfy the predicate.
-- Performs 60 cardano-node queries with `retryDelay` seconds between tries.
awaitUtxos ::
  ClusterEnv ->
  [C.AddressAny] ->
  (UTxO BabbageEra -> Bool) ->
  IO (Either AwaitWalletFundedError ())
awaitUtxos cenv addrs predicate =
  toErrorMsg <$> retrying policy checkResponse action
  where
    retryDelay = ecSlotLength $ extraConfig $ plutipConf cenv
    delay = truncate $ nominalDiffTimeToSeconds retryDelay * 1000000
    policy = constantDelay delay <> limitRetries 60

    action _ = right (not . predicate) <$> utxosAtAddresses cenv addrs

    checkResponse _ = return . fromRight True

    toErrorMsg = \case
      Left e -> Left $ AwaitingCapiError e
      Right notSatisfied ->
        if notSatisfied
          then Left AwaitingTimeoutError
          else Right ()
