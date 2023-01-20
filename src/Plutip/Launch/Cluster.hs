{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Warnings turned off intetnionally to keep module close to the original
-- as much as possible for easier maintenance.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module is modified copy of https://github.com/input-output-hk/cardano-wallet/blob/1952de13f1cd954514cfa1cb02e628cfc9fde675/lib/shelley/src/Cardano/Wallet/Shelley/Launch/Cluster.hs
-- which is
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides functions to launch cardano-nodes in a cluster for /testing/.
-- Modifications include more capabilities for cluster configuration,
-- so users can set things like slot length, epoch size, etc.
-- Alterded types and functions marked with "altered" comment.
-- Formatting and linitng checks disabled for this module for more convinisent diffs with original.

module Plutip.Launch.Cluster
    ( -- * Local test cluster launcher
      withCluster
    , LocalClusterConfig (..)
    , localClusterConfigFromEnv
    , ClusterEra (..)

      -- * Node launcher
    , NodeParams (..)
    , RunningNode (..)

      -- * Cluster node launcher
    , clusterEraFromEnv
    , clusterToApiEra
    , clusterEraToString

      -- * Configuration
    , LogFileConfig (..)
    , logFileConfigFromEnv
    , minSeverityFromEnv
    , nodeMinSeverityFromEnv
    , testMinSeverityFromEnv
    , testLogDirFromEnv
    , withSystemTempDir

      -- * Faucets
    , Credential (..)
    , sendFaucetFundsTo
    , sendFaucetAssetsTo
    , moveInstantaneousRewardsTo
    , oneMillionAda
    , genMonetaryPolicyScript

    -- * Logging
    , ClusterLog (..)

    , TempDirLog
    , LogOutput(LogToFile)

    , withLoggingNamed
) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubPublicKey )

import Cardano.Api
    ( AsType (AsStakeKey, AsStakePoolKey)
    , Key (verificationKeyHash)
    , serialiseToCBOR
    )
import Cardano.Api.Shelley
    ( AsType (AsVrfKey) )
import Cardano.Binary
    ( fromCBOR )
import System.IO.Temp
    ( createTempDirectory, getCanonicalTemporaryDirectory )
import UnliftIO.Temporary
    ( withTempDirectory )
import Cardano.BM.Data.Output
    ( ScribeDefinition (ScribeDefinition, scName, scFormat, scKind, scMinSev, scMaxSev, scPrivacy, scRotation)
    , ScribeFormat (ScText)
    , ScribeKind (FileSK, StdoutSK, StderrSK)
    , ScribePrivacy (ScPublic), ScribeId
    )
import Cardano.BM.Data.Severity
    ( Severity (Critical, Debug, Notice, Info, Error, Warning) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation, HasSeverityAnnotation (getSeverityAnnotation) )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO, liftIO )
import Cardano.CLI.Byron.Commands
    ( VerificationKeyFile (VerificationKeyFile) )
import Cardano.CLI.Shelley.Key
    ( VerificationKeyOrFile (..), readVerificationKeyOrFile )
import Cardano.Launcher
    ( LauncherLog, ProcessHasExited (..) )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , CardanoNodeConn
    , NodePort (..)
    , nodeSocketFile
    , withCardanoNode
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet)
    , NonNegativeInterval
    , PositiveUnitInterval
    , StrictMaybe (..)
    , UnitInterval
    , boundRational
    , textToUrl
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Era (Crypto) )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (sgInitialFunds, sgStaking, ShelleyGenesis, sgProtocolParams, sgNetworkMagic), ShelleyGenesisStaking (sgsPools) )
import Cardano.Pool.Metadata
    ( SMASHPoolId (..), HealthStatusSMASH )
import Cardano.Startup
    ( restrictFileMode )
import UnliftIO.Exception
    ( bracket )
import Cardano.BM.Setup ( setupTrace_, shutdown )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer, BracketLog'(BracketStart) )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
    ( Block
    , EpochNo
    , NetworkParameters
    , PoolCertificate, unEpochNo
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (Address) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin, unCoin) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (AssetId), TokenBundle (TokenBundle) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity) )
import Cardano.Wallet.Shelley.Compatibility
    ( StandardShelley, fromGenesisData )
import Cardano.Wallet.Unsafe
    ( unsafeBech32Decode )
import Cardano.Wallet.Util
    ( mapFirst )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Monad
    ( forM, forM_, liftM2, replicateM, replicateM_, void, when, (>=>) )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson
    ( object, toJSON, (.:), (.=), ToJSON, Options (fieldLabelModifier, omitNothingFields), genericToJSON, camelTo2 )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Bits
    ( (.|.) )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Char
    ( toLower )
import Data.Either
    ( fromRight, isLeft, isRight )
import Data.Foldable
    ( traverse_ )
import Data.Generics.Product.Fields
    ( setField )
import Data.IntCast
    ( intCast )
import Data.List
    ( intercalate, nub, permutations, sort )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText(toText) )
import Data.Time.Clock
    ( UTCTime, addUTCTime, getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import System.Directory
    ( copyFile, createDirectoryIfMissing, makeAbsolute )
import System.Environment
    ( getEnvironment, lookupEnv )
import System.Exit
    ( ExitCode (..), die )
import System.FilePath
    ( (<.>), (</>) )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Process.Typed
    ( ProcessConfig, proc, readProcess, setEnv, setEnvInherit )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )
import UnliftIO.Async
    ( async, link, wait )
import UnliftIO.Chan
    ( newChan, readChan, writeChan )
import UnliftIO.Exception
    ( SomeException, finally, handle, throwIO, throwString )
import UnliftIO.MVar
    ( MVar, modifyMVar, newMVar, swapMVar )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Yaml as Yaml

import Data.Default (def)
import Plutip.Launch.FaucetFunds (faucetFunds)
import GHC.TypeLits (Symbol)
import Cardano.BM.Data.LogItem (LoggerName)
import Cardano.BM.Backend.Switchboard (Switchboard)
import qualified Cardano.BM.Configuration.Model as CM
import Cardano.BM.Data.Trace (Trace)
import qualified Cardano.BM.Data.Backend as CM
import Cardano.BM.Configuration.Static (defaultConfigStdout)
import Cardano.BM.Trace (logDebug, appendName)
import Plutip.Launch.PoolConfigs (PoolRecipe (PoolRecipe, operatorKeys), defaultPoolConfigs)
import Plutip.Launch.Extra.Types
    ( ExtraConfig
    , ecSlotLength
    , ecEpochSize
    , ecMaxTxSize
    , ecRaiseExUnitsToMax
    , stdBlockExUnits
    , calculateCollateral
    , stdTxExUnits
    , maxExUnits)
data LogOutput
    = LogToStdStreams Severity
    -- ^ Log to console, with the given minimum 'Severity'.
    --
    -- Logs of Warning or higher severity will be output to stderr. Notice or
    -- lower severity logs will be output to stdout.
    | LogToFile FilePath Severity
    deriving (Eq, Show)

data ApiEra
    = ApiByron
    | ApiShelley
    | ApiAllegra
    | ApiMary
    | ApiAlonzo
    | ApiBabbage
    deriving stock (Show, Eq, Generic, Enum, Ord, Bounded)
    deriving anyclass NFData

-- | Returns the shelley test data path, which is usually relative to the
-- package sources, but can be overridden by the @SHELLEY_TEST_DATA@ environment
-- variable.
getShelleyTestDataPath :: IO FilePath
getShelleyTestDataPath = fromMaybe source <$> lookupEnvNonEmpty var
  where
    source = $(getTestData) </> "cardano-node-shelley"
    var = "SHELLEY_TEST_DATA"

logFileConfigFromEnv
    :: Maybe String
    -- ^ Optional extra subdir for TESTS_LOGDIR. E.g. @Just "alonzo"@ and
    -- @Just "mary"@ to keep them separate.
    -> IO LogFileConfig
logFileConfigFromEnv subdir = LogFileConfig
    <$> nodeMinSeverityFromEnv
    <*> (testLogDirFromEnv subdir)
    <*> pure Info

-- | The lower-case names of all 'Severity' values.
loggingSeverities :: [(String, Severity)]
loggingSeverities = [(toLower <$> show s, s) | s <- [minBound .. maxBound]]

parseLoggingSeverity :: String -> Either String Severity
parseLoggingSeverity arg =
    case lookup (map toLower arg) loggingSeverities of
        Just sev -> pure sev
        Nothing -> Left $ "unknown logging severity: " ++ arg

minSeverityFromEnv :: Severity -> String -> IO Severity
minSeverityFromEnv def var = lookupEnvNonEmpty var >>= \case
    Nothing -> pure def
    Just arg -> either die pure (parseLoggingSeverity arg)

-- Allow configuring @cardano-node@ log level with the
-- @CARDANO_NODE_TRACING_MIN_SEVERITY@ environment variable.
nodeMinSeverityFromEnv :: IO Severity
nodeMinSeverityFromEnv =
    minSeverityFromEnv Info "CARDANO_NODE_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @TESTS_TRACING_MIN_SEVERITY@ environment variable.
testMinSeverityFromEnv :: IO Severity
testMinSeverityFromEnv =
    minSeverityFromEnv Notice "TESTS_TRACING_MIN_SEVERITY"

-- | Directory for extra logging. Buildkite will set this environment variable
-- and upload logs in it automatically.
testLogDirFromEnv :: Maybe String -> IO (Maybe FilePath)
testLogDirFromEnv msubdir = do
    rel <- lookupEnvNonEmpty "TESTS_LOGDIR"
    makeAbsolute `traverse` case msubdir of
        Just subdir -> liftM2 (</>) rel (Just subdir)
        Nothing -> rel

--------------------------------------------------------------------------------
-- For Integration
--------------------------------------------------------------------------------

-- | Make a 'ProcessConfig' for running @cardano-cli@. The program must be on
-- the @PATH@, as normal. Sets @CARDANO_NODE_SOCKET_PATH@ for the subprocess, if
-- a 'CardanoNodeConn' is provided.
cliConfigBase
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> Maybe CardanoNodeConn -- ^ optional cardano-node socket path
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigBase tr conn args = do
    traceWith tr (MsgCLI args)
    env <- getEnvironment
    let mkEnv c = ("CARDANO_NODE_SOCKET_PATH", nodeSocketFile c):env
    let cliEnv = maybe setEnvInherit (setEnv . mkEnv) conn
    pure $ cliEnv $ proc "cardano-cli" args

cliConfigNode
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> CardanoNodeConn -- ^ cardano-node socket path
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfigNode tr conn = cliConfigBase tr (Just conn)

cliConfig
    :: Tracer IO ClusterLog -- ^ for logging the command
    -> [String] -- ^ command-line arguments
    -> IO (ProcessConfig () () ())
cliConfig tr = cliConfigBase tr Nothing

-- | A quick helper to interact with the 'cardano-cli'. Assumes the cardano-cli
-- is available in PATH.
cli :: Tracer IO ClusterLog -> [String] -> IO ()
cli tr = cliConfig tr >=> void . readProcessStdoutOrFail

cliLine :: Tracer IO ClusterLog -> [String] -> IO String
cliLine tr = cliConfig tr >=>
    fmap (BL8.unpack . getFirstLine) . readProcessStdoutOrFail

readProcessStdoutOrFail :: ProcessConfig () () () -> IO BL.ByteString
readProcessStdoutOrFail processConfig = do
    (st, out, err) <- readProcess processConfig
    case st of
        ExitSuccess -> pure out
        ExitFailure _ -> throwIO $ userError $ mconcat
            [ "command failed: "
            , BL8.unpack err
            ]

getFirstLine :: BL8.ByteString -> BL8.ByteString
getFirstLine = BL8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@.
cliRetry
    :: Tracer IO ClusterLog
    -> Text -- ^ message to print before running command
    -> ProcessConfig () a b
    -> IO ()
cliRetry tr msg processConfig = do
    (st, out, err) <- retrying pol (const isFail) (const cmd)
    traceWith tr $ MsgCLIStatus msg st out err
    case st of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwIO $ ProcessHasExited
            ("cardano-cli failed: " <> BL8.unpack err) st
  where
    cmd = do
        traceWith tr $ MsgCLIRetry msg
        (st, out, err) <- readProcess processConfig
        case st of
            ExitSuccess -> pure ()
            ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
        pure (st, out, err)
    isFail (st, _, _) = pure (st /= ExitSuccess)
    pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

-- | Represents the notion of a fully configured pool. All keys are known, but
-- not necessarily exposed using this interface.
data ConfiguredPool = ConfiguredPool
    { operatePool
        :: forall a. NodeParams -> (RunningNode -> IO a) -> IO a
      -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
      -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: IO (ShelleyGenesis StandardShelley -> ShelleyGenesis StandardShelley)
    , registerViaTx :: RunningNode -> IO ()
    }

data PoolMetadataServer = PoolMetadataServer
    { registerMetadataForPoolIndex :: Int -> Aeson.Value -> IO ()
    , urlFromPoolIndex  :: Int -> String
    }

withPoolMetadataServer
    :: Tracer IO ClusterLog
    -> FilePath
    -> (PoolMetadataServer -> IO a)
    -> IO a
withPoolMetadataServer tr dir action = do
    let metadir = dir </> "pool-metadata"
    createDirectoryIfMissing False metadir
    withStaticServer metadir $ \baseURL -> do
        let _urlFromPoolIndex i = baseURL </> metadataFileName i
        action $ PoolMetadataServer
            { registerMetadataForPoolIndex = \i metadata -> do
                let metadataBytes = Aeson.encode metadata
                BL8.writeFile (metadir </> (metadataFileName i)) metadataBytes
                let hash = blake2b256 (BL.toStrict metadataBytes)
                traceWith tr $
                    MsgRegisteringPoolMetadata
                        (_urlFromPoolIndex i)
                        (B8.unpack $ hex hash)
            , urlFromPoolIndex = _urlFromPoolIndex
            }
  where

    metadataFileName :: Int -> FilePath
    metadataFileName i = show i <> ".json"

configurePools
    :: Tracer IO ClusterLog
    -> FilePath
    -> PoolMetadataServer
    -> [PoolRecipe]
    -> IO [ConfiguredPool]
configurePools tr dir metadataServer =
    mapM (configurePool tr dir metadataServer)

configurePool
    :: Tracer IO ClusterLog
    -> FilePath
    -> PoolMetadataServer
    -> PoolRecipe
    -> IO ConfiguredPool
configurePool tr baseDir metadataServer recipe = do
    let PoolRecipe pledgeAmt i mretirementEpoch metadata _ _ = recipe

    -- Use pool-specific dir
    let name = "pool-" <> show i
    let dir = baseDir </> name
    createDirectoryIfMissing False dir

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair tr dir
    (kesPrv, kesPub) <- genKesKeyPair tr dir
    (opPrv, opPub, opCount) <- writeOperatorKeyPair tr dir recipe
    opCert <- issueOpCert tr dir kesPub opPrv opCount
    let ownerPub = dir </> "stake.pub"
    let ownerPrv = dir </> "stake.prv"
    genStakeAddrKeyPair tr (ownerPrv, ownerPub)

    let metadataURL = urlFromPoolIndex metadataServer i
    registerMetadataForPoolIndex metadataServer i metadata
    let metadataBytes = Aeson.encode metadata

    pure $ ConfiguredPool
        { operatePool = \nodeParams action -> do

            let NodeParams genesisFiles hardForks (port, peers) logCfg = nodeParams
            let logCfg' = setLoggingName name logCfg

            topology <- genTopology dir peers
            withStaticServer dir $ \url -> do
                traceWith tr $ MsgStartedStaticServer dir url

                (config, block0, bp, vd, genesisPools)
                    <- genNodeConfig
                        dir
                        ""
                        genesisFiles
                        hardForks
                        logCfg'

                let cfg = CardanoNodeConfig
                        { nodeDir = dir
                        , nodeConfigFile = config
                        , nodeTopologyFile = topology
                        , nodeDatabaseDir = "db"
                        , nodeDlgCertFile = Nothing
                        , nodeSignKeyFile = Nothing
                        , nodeOpCertFile = Just opCert
                        , nodeKesKeyFile = Just kesPrv
                        , nodeVrfKeyFile = Just vrfPrv
                        , nodePort = Just (NodePort port)
                        , nodeLoggingHostname = Just name
                        }
                withCardanoNodeProcess tr name cfg $ \socket -> do
                    -- Here is our chance to respect the 'retirementEpoch' of
                    -- the 'PoolRecipe'.
                    --
                    -- NOTE: We also submit the retirement cert in
                    -- @registerViaTx@, but this seems to work regardless. (We
                    -- do want to submit it here for the sake of babbage)
                    let retire e = do
                            retCert <- issuePoolRetirementCert tr dir opPub e
                            (rawTx, faucetPrv) <- preparePoolRetirement tr dir [retCert]
                            tx <- signTx tr dir rawTx [faucetPrv, ownerPrv, opPrv]
                            submitTx tr socket "retirement cert" tx

                    traverse_ retire mretirementEpoch

                    action $ RunningNode socket block0 (bp, vd) genesisPools

        , registerViaShelleyGenesis = do
            poolId <- stakePoolIdFromOperatorVerKey opPub
            vrf <- poolVrfFromFile vrfPub
            stakePubHash <- stakingKeyHashFromFile ownerPub
            pledgeAddr <- stakingAddrFromVkFile ownerPub

            let params = Ledger.PoolParams
                  { _poolId = poolId
                  , _poolVrf = vrf
                  , _poolPledge = Ledger.Coin $ intCast pledgeAmt
                  , _poolCost = Ledger.Coin 0
                  , _poolMargin = unsafeUnitInterval 0.1
                  , _poolRAcnt = Ledger.RewardAcnt Mainnet $ Ledger.KeyHashObj stakePubHash
                  , _poolOwners = Set.fromList [stakePubHash]
                  , _poolRelays = mempty
                  , _poolMD = SJust $ Ledger.PoolMetadata
                        (fromMaybe (error "invalid url (too long)")
                            $ textToUrl
                            $ T.pack metadataURL)
                        (blake2b256 (BL.toStrict metadataBytes))
                  }

            let updateStaking = \sgs -> sgs
                    { Ledger.sgsPools = (Map.singleton poolId params)
                        <> (sgsPools sgs)
                    , Ledger.sgsStake = (Map.singleton stakePubHash poolId)
                        <> Ledger.sgsStake sgs
                    }
            let poolSpecificFunds = Map.fromList
                    [(pledgeAddr, Ledger.Coin $ intCast pledgeAmt)]
            return $ \sg -> sg
                { sgInitialFunds = poolSpecificFunds <> (sgInitialFunds sg)
                , sgStaking = updateStaking (sgStaking sg)
                }
        , registerViaTx = \(RunningNode socket _ _ _) -> do
            stakeCert <- issueStakeVkCert tr dir "stake-pool" ownerPub
            let poolRegistrationCert = dir </> "pool.cert"
            cli tr
                [ "stake-pool", "registration-certificate"
                , "--cold-verification-key-file", opPub
                , "--vrf-verification-key-file", vrfPub
                , "--pool-pledge", show pledgeAmt
                , "--pool-cost", "0"
                , "--pool-margin", "0.1"
                , "--pool-reward-account-verification-key-file", ownerPub
                , "--pool-owner-stake-verification-key-file", ownerPub
                , "--metadata-url", metadataURL
                , "--metadata-hash", blake2b256S (BL.toStrict metadataBytes)
                , "--mainnet"
                , "--out-file", poolRegistrationCert
                ]

            mPoolRetirementCert <- traverse
                (issuePoolRetirementCert tr dir opPub) mretirementEpoch
            dlgCert <- issueDlgCert tr dir ownerPub opPub

            -- In order to get a working stake pool we need to.
            --
            -- 1. Register a stake key for our pool.
            -- 2. Register the stake pool
            -- 3. Delegate funds to our pool's key.
            --
            -- We cheat a bit here by delegating to our stake address right away
            -- in the transaction used to registered the stake key and the pool
            -- itself.  Thus, in a single transaction, we end up with a
            -- registered pool with some stake!

            let certificates = catMaybes
                    [ pure stakeCert
                    , pure poolRegistrationCert
                    , pure dlgCert
                    , mPoolRetirementCert
                    ]
            (rawTx, faucetPrv) <- preparePoolRegistration
                tr dir ownerPub certificates pledgeAmt
            tx <- signTx tr dir rawTx [faucetPrv, ownerPrv, opPrv]
            submitTx tr socket name tx
        , metadataUrl = T.pack metadataURL
        , recipe = recipe
        }

-- altered: `def :: ExtraConfig` added
localClusterConfigFromEnv :: IO LocalClusterConfig
localClusterConfigFromEnv = do
    era <- clusterEraFromEnv
    logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
    pure $ LocalClusterConfig defaultPoolConfigs era logConf def

data ClusterEra
    = ByronNoHardFork
    | ShelleyHardFork
    | AllegraHardFork
    | MaryHardFork
    | AlonzoHardFork
    | BabbageHardFork
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Convert @ClusterEra@ to a @ApiEra@.
clusterToApiEra :: ClusterEra -> ApiEra
clusterToApiEra = \case
    ByronNoHardFork -> ApiByron
    ShelleyHardFork -> ApiShelley
    AllegraHardFork -> ApiAllegra
    MaryHardFork -> ApiMary
    AlonzoHardFork -> ApiAlonzo
    BabbageHardFork -> ApiBabbage

-- | Defaults to the latest era.
clusterEraFromEnv :: IO ClusterEra
clusterEraFromEnv =
    fmap withDefault . traverse getEra =<< lookupEnvNonEmpty var
  where
    var = "LOCAL_CLUSTER_ERA"
    getEra env = case map toLower env of
        "byron" -> pure ByronNoHardFork
        "shelley" -> pure ShelleyHardFork
        "allegra" -> pure AllegraHardFork
        "mary" -> pure MaryHardFork
        "alonzo" -> pure AlonzoHardFork
        "babbage" -> pure BabbageHardFork
        _ -> die $ var ++ ": unknown era"
    withDefault = fromMaybe maxBound

clusterEraToString :: ClusterEra -> String
clusterEraToString = \case
    ByronNoHardFork -> "byron"
    ShelleyHardFork -> "shelley"
    AllegraHardFork -> "allegra"
    MaryHardFork    -> "mary"
    AlonzoHardFork  -> "alonzo"
    BabbageHardFork -> "babbage"

-- altered: `cfgExtraConfig :: ExtraConfig` added
data LocalClusterConfig = LocalClusterConfig
    { cfgStakePools :: [PoolRecipe]
    -- ^ Stake pools to register.
    , cfgLastHardFork :: ClusterEra
    -- ^ Which era to use.
    , cfgNodeLogging :: LogFileConfig
    -- ^ Log severity for node.
    , cfgExtraConfig :: ExtraConfig
    } deriving stock (Show)

-- | Information about a launched node.
data RunningNode = RunningNode
    CardanoNodeConn
    -- ^ Socket path
    Block
    -- ^ Genesis block
    (NetworkParameters, NodeToClientVersionData)
    [PoolCertificate]
    -- ^ Shelley genesis pools
    deriving stock (Show, Eq)


unsafeUnitInterval :: Rational -> UnitInterval
unsafeUnitInterval x = fromMaybe
        (error $ "unsafeUnitInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafeNonNegativeInterval :: Rational -> NonNegativeInterval
unsafeNonNegativeInterval x = fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)

unsafePositiveUnitInterval :: Rational -> PositiveUnitInterval
unsafePositiveUnitInterval x = fromMaybe
        (error $ "unsafeNonNegativeInterval: " <> show x <> " is out of bounds")
        (boundRational x)


-- altered
generateGenesis
    :: FilePath
    -> UTCTime
    -> [(Address, Coin)]
    -> (ShelleyGenesis StandardShelley -> ShelleyGenesis StandardShelley)
       -- ^ For adding genesis pools and staking in Babbage and later.
    -> ExtraConfig -- <- alterd by adding `ExtraConfig` to arguments
    -> IO GenesisFiles
generateGenesis dir systemStart initialFunds addPoolsToGenesis extraConf = do
    source <- getShelleyTestDataPath
    let (maxTxExUnits, maxBlockExUnits) = if ecRaiseExUnitsToMax extraConf
           then (maxExUnits, maxExUnits)
           else (stdTxExUnits, stdBlockExUnits)
        collateral = calculateCollateral $ ecMaxTxSize extraConf
    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "alonzo-genesis.yaml")
        >>= withAddedKey "maxTxExUnits" maxTxExUnits
        >>= withAddedKey "maxBlockExUnits" maxBlockExUnits
        >>= withAddedKey "collateralPercentage" collateral
        >>= Aeson.encodeFile (dir </> "genesis.alonzo.json")

    let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart
    let systemStart' = posixSecondsToUTCTime . fromRational . toRational $ startTime

    let pparams = Ledger.PParams
            { _minfeeA = 100
            , _minfeeB = 100000
            , _minUTxOValue = Ledger.Coin 1_000_000

            , _keyDeposit = Ledger.Coin 1_000_000
            , _poolDeposit = Ledger.Coin 0

            , _maxBBSize = 239857
            , _maxBHSize = 217569
            , _maxTxSize = ecMaxTxSize extraConf

            , _minPoolCost = Ledger.Coin 0

            , _extraEntropy = Ledger.NeutralNonce

            -- There are a few smaller features/fixes which are enabled based on
            -- the protocol version rather than just the era, so we need to
            -- set it to a realisitic value.
            , _protocolVersion = Ledger.ProtVer 8 0

            -- Sensible pool & reward parameters:
            , _nOpt = 3
            , _rho = unsafeUnitInterval 0.178650067
            , _tau = unsafeUnitInterval 0.1
            , _a0 = unsafeNonNegativeInterval 0.1
            , _d = unsafeUnitInterval 0

            -- The epoch bound on pool retirements specifies how many epochs
            -- in advance retirements may be announced. For testing purposes,
            -- we allow retirements to be announced far into the future.
            ,  _eMax = 1000000
            }

    let sg = addPoolsToGenesis $ ShelleyGenesis
            { sgSystemStart = systemStart'
            , sgActiveSlotsCoeff = unsafePositiveUnitInterval 0.5
            , sgSlotLength = ecSlotLength extraConf
            , sgSecurityParam = 5
            , sgEpochLength = ecEpochSize extraConf
            , sgUpdateQuorum = 1
            , sgNetworkMagic = 764824073
            , sgSlotsPerKESPeriod = 86400
            , sgMaxKESEvolutions = 5
            , sgNetworkId = Mainnet
            , sgMaxLovelaceSupply = 1000000000000000000
            , sgProtocolParams = pparams
            , sgInitialFunds = extraInitialFunds
            , sgStaking = Ledger.emptyGenesisStaking

            -- We need this to submit MIR certs (and probably for the BFT node
            -- pre-babbage):
            , sgGenDelegs = fromRight (error "invalid sgGenDelegs") $ Aeson.eitherDecode $ Aeson.encode [aesonQQ| {
                    "8ae01cab15f6235958b1147e979987bbdb90788f7c4e185f1632427a": {
                        "delegate": "b7bf59bb963aa785afe220f5b0d3deb826fd0bcaeeee58cb81ab443d",
                        "vrf": "4ebcf8b4c13c24d89144d72f544d1c425b4a3aa1ace30af4eb72752e75b40d3e"
                    }
                }
                |]
            }

    let shelleyGenesisFile = (dir </> "genesis.json")
    Aeson.encodeFile shelleyGenesisFile sg

    let byronGenesisFile = dir </> "genesis.byron.json"
    Yaml.decodeFileThrow @_ @Aeson.Value (source </> "byron-genesis.yaml")
        >>= withAddedKey "startTime" startTime
        >>= Aeson.encodeFile byronGenesisFile

    return $ GenesisFiles
        { byronGenesis = byronGenesisFile
        , shelleyGenesis = dir </> "genesis.json"
        , alonzoGenesis = dir </> "genesis.alonzo.json"
        }

  where
    extraInitialFunds :: Map (Ledger.Addr (Crypto StandardShelley)) Ledger.Coin
    extraInitialFunds = Map.fromList
        [ (fromMaybe (error "extraFunds: invalid addr") $ Ledger.deserialiseAddr addrBytes
         , Ledger.Coin $ intCast c)
        | (Address addrBytes, Coin c) <- initialFunds
        ]

-- | Execute an action after starting a cluster of stake pools. The cluster also
-- contains a single BFT node that is pre-configured with keys available in the
-- test data.
--
-- This BFT node is essential in order to bootstrap the chain and allow
-- registering pools. Passing `0` as a number of pool will simply start a single
-- BFT node.
--
-- The cluster is configured to automatically hard fork to Shelley at epoch 1
-- and then to Allegra at epoch 2. Callback actions can be provided to run
-- a little time after the hard forks are scheduled.
--
-- The callback actions are not guaranteed to use the same node.
withCluster
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging.
    -> FilePath
    -- ^ Temporary directory to create config files in.
    -> LocalClusterConfig
    -- ^ The configurations of pools to spawn.
    -> [(Address, Coin)] -- Faucet funds
    -> (RunningNode -> IO a)
    -- ^ Action to run once when the stake pools are setup.
    -> IO a
withCluster tr dir LocalClusterConfig{..} initialFunds onClusterStart = bracketTracer' tr "withCluster" $ do
    withPoolMetadataServer tr dir $ \metadataServer -> do
        createDirectoryIfMissing True dir
        traceWith tr $ MsgStartingCluster dir
        resetGlobals
        putClusterEra dir cfgLastHardFork

        systemStart <- addUTCTime 1 <$> getCurrentTime
        configuredPools <- configurePools tr dir metadataServer cfgStakePools

        addGenesisPools <- do
            genesisDeltas <- mapM registerViaShelleyGenesis configuredPools
            pure $ foldr (.) id genesisDeltas
        let federalizeNetwork =
                let
                    adjustPParams f genesis = genesis
                        { sgProtocolParams = f (sgProtocolParams genesis) }
                in
                    adjustPParams (setField @"_d" (unsafeUnitInterval 0.25))

        genesisFiles <- generateGenesis
            dir
            systemStart
            (initialFunds <> faucetFunds)
            (if postAlonzo then addGenesisPools else federalizeNetwork)
            cfgExtraConfig

        if postAlonzo
        then do
            ports <- rotate <$> randomUnusedTCPPorts nPools
            launchPools configuredPools genesisFiles ports onClusterStart'
        else do
            ports <- rotate <$> randomUnusedTCPPorts (1 + nPools)
            let bftCfg = NodeParams
                    genesisFiles
                    cfgLastHardFork
                    (head ports)
                    cfgNodeLogging
            withBFTNode tr dir bftCfg $ \runningBFTNode -> do
                -- NOTE: We used to perform 'registerViaTx' as part of 'launchPools'
                -- where we waited for the pools to become active (e.g. be in
                -- the stake distribution) in parallel. Just submitting the
                -- registration certs in sequence /seems/ to work though, and the
                -- setup working 100% correctly in alonzo will soon not be
                -- important.
                mapM_ (`registerViaTx` runningBFTNode) configuredPools
                launchPools configuredPools genesisFiles (tail ports) onClusterStart'
  where
    nPools = length cfgStakePools

    postAlonzo = cfgLastHardFork >= BabbageHardFork

    onClusterStart' node@(RunningNode socket _ _ _) = do
        (rawTx, faucetPrv) <- prepareKeyRegistration tr dir
        tx <- signTx tr dir rawTx [faucetPrv]
        submitTx tr socket "pre-registered stake key" tx
        onClusterStart node

    -- | Actually spin up the pools.
    launchPools
        :: [ConfiguredPool]
        -> GenesisFiles
        -> [(Int, [Int])]
        -- @(port, peers)@ pairs availible for the nodes. Can be used to e.g.
        -- add a BFT node as extra peer for all pools.
        -> (RunningNode -> IO a)
        -- ^ Action to run once when the stake pools are setup.
        -> IO a
    launchPools configuredPools genesisFiles ports action = do
        waitGroup <- newChan
        doneGroup <- newChan

        let poolCount = length configuredPools

        let waitAll = do
                traceWith tr $
                    MsgDebug "waiting for stake pools to register"
                replicateM poolCount (readChan waitGroup)

        let onException :: SomeException -> IO ()
            onException e = do
                traceWith tr $
                    MsgDebug $ "exception while starting pool: " <>
                    T.pack (show e)
                writeChan waitGroup (Left e)

        let mkConfig (port, peers) =
                NodeParams
                genesisFiles
                cfgLastHardFork
                (port, peers)
                cfgNodeLogging
        asyncs <- forM (zip configuredPools ports) $
            \(configuredPool, (port, peers)) -> do
                async $ handle onException $ do
                    let cfg = mkConfig (port, peers)
                    operatePool configuredPool cfg $ \runningPool -> do
                            writeChan waitGroup $ Right runningPool
                            readChan doneGroup
        mapM_ link asyncs
        let cancelAll = do
                traceWith tr $ MsgDebug "stopping all stake pools"
                replicateM_ poolCount (writeChan doneGroup ())
                mapM_ wait asyncs

        traceWith tr $ MsgRegisteringStakePools poolCount
        group <- waitAll
        if length (filter isRight group) /= poolCount then do
            cancelAll
            let errors = show (filter isLeft group)
            throwIO $ ProcessHasExited
                ("cluster didn't start correctly: " <> errors)
                (ExitFailure 1)
        else do
            -- Run the action using the connection to the first pool
            let firstPool = either (error . show) id $ head group
            action firstPool `finally` cancelAll


    -- | Get permutations of the size (n-1) for a list of n elements, alongside
    -- with the element left aside. `[a]` is really expected to be `Set a`.
    --
    -- >>> rotate [1,2,3]
    -- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    rotate :: Ord a => [a] -> [(a, [a])]
    rotate = nub . fmap (\(x:xs) -> (x, sort xs)) . permutations

data LogFileConfig = LogFileConfig
    { minSeverityTerminal :: Severity
      -- ^ Minimum logging severity
    , extraLogDir :: Maybe FilePath
      -- ^ Optional additional output to log file
    , minSeverityFile :: Severity
      -- ^ Minimum logging severity for 'extraLogFile'
    } deriving stock (Show)

-- | Configuration parameters which update the @node.config@ test data file.
data NodeParams = NodeParams
    { nodeGenesisFiles :: GenesisFiles
      -- ^ Genesis block start time
    , nodeHardForks :: ClusterEra
      -- ^ Era to hard fork into.
    , nodePeers :: (Int, [Int])
      -- ^ A list of ports used by peers and this node
    , nodeLogConfig :: LogFileConfig
      -- ^ The node will always log to "cardano-node.log" relative to the
      -- config. This option can set the minimum severity and add another output
      -- file.
    } deriving (Show)

withBFTNode
    :: Tracer IO ClusterLog
    -- ^ Trace for subprocess control logging
    -> FilePath
    -- ^ Parent state directory. Node data will be created in a subdirectory of
    -- this.
    -> NodeParams
    -- ^ Parameters used to generate config files.
    -> (RunningNode -> IO a)
    -- ^ Callback function with genesis parameters
    -> IO a
withBFTNode tr baseDir params action =
    bracketTracer' tr "withBFTNode" $ do
        createDirectoryIfMissing False dir
        source <- getShelleyTestDataPath

        let copyKeyFile f = do
                let dst = dir </> f
                copyFile (source </> f) dst
                restrictFileMode dst
                pure dst

        [bftCert, bftPrv, vrfPrv, kesPrv, opCert] <- forM
            [ "bft-leader" <> ".byron.cert"
            , "bft-leader" <> ".byron.skey"
            , "bft-leader" <> ".vrf.skey"
            , "bft-leader" <> ".kes.skey"
            , "bft-leader" <> ".opcert"
            ]
            copyKeyFile

        (config, block0, networkParams, versionData, genesisPools)
            <- genNodeConfig dir "-bft" genesisFiles hardForks (setLoggingName name logCfg)
        topology <- genTopology dir peers

        let cfg = CardanoNodeConfig
                { nodeDir = dir
                , nodeConfigFile = config
                , nodeTopologyFile = topology
                , nodeDatabaseDir = "db"
                , nodeDlgCertFile = Just bftCert
                , nodeSignKeyFile = Just bftPrv
                , nodeOpCertFile = Just opCert
                , nodeKesKeyFile = Just kesPrv
                , nodeVrfKeyFile = Just vrfPrv
                , nodePort = Just (NodePort port)
                , nodeLoggingHostname = Just name
                }

        withCardanoNodeProcess tr name cfg $ \socket ->
            action $ RunningNode socket block0 (networkParams, versionData) genesisPools
  where
    name = "bft"
    dir = baseDir </> name
    NodeParams genesisFiles hardForks (port, peers) logCfg = params

withCardanoNodeProcess
    :: Tracer IO ClusterLog
    -> String
    -> CardanoNodeConfig
    -> (CardanoNodeConn -> IO a)
    -> IO a
withCardanoNodeProcess tr name cfg = withCardanoNode tr' cfg >=> throwErrs
  where
    tr' = contramap (MsgLauncher name) tr
    throwErrs = either throwIO pure

setLoggingName :: String -> LogFileConfig -> LogFileConfig
setLoggingName name cfg = cfg { extraLogDir = filename <$> extraLogDir cfg }
    where filename = (</> (name <.> "log"))

data GenesisFiles = GenesisFiles
    { byronGenesis :: FilePath
    , shelleyGenesis :: FilePath
    , alonzoGenesis :: FilePath
    } deriving stock (Show, Eq)

genNodeConfig
    :: FilePath
    -- ^ A top-level directory where to put the configuration.
    -> String -- Node name
    -> GenesisFiles
    -- ^ Genesis block start time
    -> ClusterEra
    -- ^ Last era to hard fork into.
    -> LogFileConfig
    -- ^ Minimum severity level for logging and optional /extra/ logging output
    -> IO (FilePath, Block, NetworkParameters, NodeToClientVersionData, [PoolCertificate])
genNodeConfig dir name genesisFiles clusterEra logCfg = do
    let LogFileConfig severity mExtraLogFile extraSev = logCfg
    let GenesisFiles{byronGenesis,shelleyGenesis,alonzoGenesis} = genesisFiles

    source <- getShelleyTestDataPath

    let fileScribe (path, sev) = ScribeDefinition
                { scName = path
                , scFormat = ScText
                , scKind = FileSK
                , scMinSev = sev
                , scMaxSev = Critical
                , scPrivacy = ScPublic
                , scRotation = Nothing
                }

    let scribes = map fileScribe $ catMaybes
            [ Just ("cardano-node.log", severity)
            , (, extraSev) . T.pack <$> mExtraLogFile
            ]

    ----
    -- Configuration
    Yaml.decodeFileThrow (source </> "node.config")
        >>= withAddedKey "ShelleyGenesisFile" shelleyGenesis
        >>= withAddedKey "ByronGenesisFile" byronGenesis
        >>= withAddedKey "AlonzoGenesisFile" alonzoGenesis
        >>= withHardForks clusterEra
        >>= withAddedKey "minSeverity" Debug
        >>= withScribes scribes
        >>= withObject (addMinSeverityStdout severity)
        >>= Yaml.encodeFile (dir </> ("node" <> name <> ".config"))


    -- Parameters
    sg <- Yaml.decodeFileThrow
        @_ @(ShelleyGenesis StandardShelley) shelleyGenesis

    let (np, block0, genesisPools) = fromGenesisData sg
    let networkMagic = sgNetworkMagic sg
    let versionData = NodeToClientVersionData $ NetworkMagic networkMagic

    pure
        ( dir </> ("node" <> name <> ".config")
        , block0
        , np
        , versionData
        , genesisPools
        )
  where
    withScribes scribes =
        withAddedKey "setupScribes" scribes
        >=> withAddedKey "defaultScribes"
            (map (\s -> [toJSON $ scKind s, toJSON $ scName s]) scribes)

    withHardForks era =
        withObject (pure . Aeson.union (Aeson.fromList hardForks))
      where
        hardForks =
            [ (Aeson.fromText $ "Test" <> T.pack (show hardFork) <> "AtEpoch"
              , Yaml.Number 0
              )
            | hardFork <- [ShelleyHardFork .. era]
            ]

withAddedKey
    :: (MonadFail m, Yaml.ToJSON a)
    => Aeson.Key
    -> a
    -> Aeson.Value
    -> m Aeson.Value
withAddedKey k v = withObject (pure . Aeson.insert k (toJSON v))

-- | Generate a topology file from a list of peers.
genTopology :: FilePath -> [Int] -> IO FilePath
genTopology dir peers = do
    let file = dir </> "node.topology"
    Aeson.encodeFile file $ Aeson.object [ "Producers" .= map encodePeer peers ]
    pure file
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port = Aeson.object
        [ "addr"    .= ("127.0.0.1" :: String)
        , "port"    .= port
        , "valency" .= (1 :: Int)
        ]
-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: Tracer IO ClusterLog
    -> FilePath
    -> PoolRecipe
    -> IO (FilePath, FilePath, FilePath)
writeOperatorKeyPair tr dir recipe = do
    let (_pId, pub, prv, count) = operatorKeys recipe

    traceWith tr $ MsgGenOperatorKeyPair dir

    let opPub = dir </> "op.pub"
    let opPrv = dir </> "op.prv"
    let opCount = dir </> "op.count"

    Aeson.encodeFile opPub pub
    Aeson.encodeFile opPrv prv
    Aeson.encodeFile opCount count

    pure (opPrv, opPub, opCount)

-- | Create a key pair for a node KES operational key
genKesKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genKesKeyPair tr dir = do
    let kesPub = dir </> "kes.pub"
    let kesPrv = dir </> "kes.prv"
    cli tr
        [ "node", "key-gen-KES"
        , "--verification-key-file", kesPub
        , "--signing-key-file", kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair :: Tracer IO ClusterLog -> FilePath -> IO (FilePath, FilePath)
genVrfKeyPair tr dir = do
    let vrfPub = dir </> "vrf.pub"
    let vrfPrv = dir </> "vrf.prv"
    cli tr
        [ "node", "key-gen-VRF"
        , "--verification-key-file", vrfPub
        , "--signing-key-file", vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Create a stake address key pair
genStakeAddrKeyPair :: Tracer IO ClusterLog -> (FilePath, FilePath) -> IO ()
genStakeAddrKeyPair tr (stakePrv, stakePub)= do
    cli tr
        [ "stake-address", "key-gen"
        , "--verification-key-file", stakePub
        , "--signing-key-file", stakePrv
        ]

-- | Issue a node operational certificate
issueOpCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
issueOpCert tr dir kesPub opPrv opCount = do
    let file = dir </> "op.cert"
    cli tr
        [ "node", "issue-op-cert"
        , "--kes-verification-key-file", kesPub
        , "--cold-signing-key-file", opPrv
        , "--operational-certificate-issue-counter-file", opCount
        , "--kes-period", "0"
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> String
    -> FilePath
    -> IO FilePath
issueStakeVkCert tr dir prefix stakePub = do
    let file = dir </> prefix <> "-stake.cert"
    cli tr
        [ "stake-address", "registration-certificate"
        , "--staking-verification-key-file", stakePub
        , "--out-file", file
        ]
    pure file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> String
    -> FilePath
    -> IO FilePath
issueStakeScriptCert tr dir prefix stakeScript = do
    let file = dir </> prefix <> "-stake.cert"
    cli tr
        [ "stake-address", "registration-certificate"
        , "--stake-script-file", stakeScript
        , "--out-file", file
        ]
    pure file


stakePoolIdFromOperatorVerKey
    :: FilePath -> IO (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsStakePoolKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: FilePath -> IO (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsVrfKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: FilePath -> IO (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsStakeKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure $ either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: FilePath -> IO (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile filepath = do
    stakePoolVerKey <- either (error . show) id <$> readVerificationKeyOrFile AsStakeKey
        (VerificationKeyFilePath $ VerificationKeyFile filepath)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    let payKH = either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    let delegKH = either (error . show) snd $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    return $ Ledger.Addr Mainnet
        (Ledger.KeyHashObj payKH)
        (Ledger.StakeRefBase (Ledger.KeyHashObj delegKH))

issuePoolRetirementCert
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> EpochNo
    -> IO FilePath
issuePoolRetirementCert tr dir opPub retirementEpoch = do
    let file  = dir </> "pool-retirement.cert"
    cli tr
        [ "stake-pool", "deregistration-certificate"
        , "--cold-verification-key-file", opPub
        , "--epoch", show (unEpochNo retirementEpoch)
        , "--out-file", file
        ]
    pure file

-- | Create a stake address delegation certificate.
issueDlgCert :: Tracer IO ClusterLog -> FilePath -> FilePath -> FilePath -> IO FilePath
issueDlgCert tr dir stakePub opPub = do
    let file = dir </> "dlg.cert"
    cli tr
        [ "stake-address", "delegation-certificate"
        , "--staking-verification-key-file", stakePub
        , "--stake-pool-verification-key-file", opPub
        , "--out-file", file
        ]
    pure file

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
preparePoolRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> FilePath
    -> [FilePath]
    -> Integer
    -> IO (FilePath, FilePath)
preparePoolRegistration tr dir stakePub certs pledgeAmt = do
    let file = dir </> "tx.raw"
    addr <- genSinkAddress tr dir (Just stakePub)
    (faucetInput, faucetPrv) <- takeFaucet
    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", addr <> "+" <> show pledgeAmt
        , "--ttl", "400"
        , "--fee", show (faucetAmt - pledgeAmt - depositAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

preparePoolRetirement
    :: Tracer IO ClusterLog
    -> FilePath
    -> [FilePath]
    -> IO (FilePath, FilePath)
preparePoolRetirement tr dir certs = do
    let file = dir </> "tx.raw"
    (faucetInput, faucetPrv) <- takeFaucet
    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "400"
        , "--fee", show (faucetAmt)
        , "--out-file", file
        ] ++ mconcat ((\cert -> ["--certificate-file",cert]) <$> certs)

    pure (file, faucetPrv)

-- | For creating test fixtures. Returns PolicyId, signing key, and verification
-- key hash, all hex-encoded. Files are put in the given directory.
genMonetaryPolicyScript
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Directory
    -> IO (String, (String, String))
genMonetaryPolicyScript tr dir = do
    let policyPub = dir </> "policy.pub"
    let policyPrv = dir </> "policy.prv"

    cli tr
        [ "address", "key-gen"
        , "--verification-key-file", policyPub
        , "--signing-key-file", policyPrv
        ]
    skey <- T.unpack <$> readKeyFromFile policyPrv
    vkeyHash <- cliLine tr
        [ "address", "key-hash"
        , "--payment-verification-key-file", policyPub
        ]
    script <- writeMonetaryPolicyScriptFile dir vkeyHash
    policyId <- cliLine tr
        [ "transaction", "policyid"
        , "--script-file", script
        ]

    pure (policyId, (skey, vkeyHash))

writeMonetaryPolicyScriptFile
    :: FilePath -- ^ Destination directory for script file
    -> String -- ^ The script verification key hash
    -> IO FilePath -- ^ Returns the filename written
writeMonetaryPolicyScriptFile dir keyHash = do
    let scriptFile = dir </> keyHash <.> "script"
    Aeson.encodeFile scriptFile $ object
        [ "type" .= Aeson.String "sig"
        , "keyHash" .= keyHash
        ]
    pure scriptFile

writePolicySigningKey
    :: FilePath -- ^ destination directory for key file
    -> String -- ^ Name of file, keyhash perhaps.
    -> String -- ^ The cbor-encoded key material, encoded in hex
    -> IO FilePath -- ^ Returns the filename written
writePolicySigningKey dir keyHash cborHex = do
    let keyFile = dir </> keyHash <.> "skey"
    Aeson.encodeFile keyFile $ object
        [ "type" .= Aeson.String "PaymentSigningKeyShelley_ed25519"
        , "description" .= Aeson.String "Payment Signing Key"
        , "cborHex" .= cborHex
        ]
    pure keyFile

-- | Dig in to a @cardano-cli@ TextView key file to get the hex-encoded key.
readKeyFromFile :: FilePath -> IO Text
readKeyFromFile f = do
    textView <- either throwString pure =<< Aeson.eitherDecodeFileStrict' f
    either throwString pure $ Aeson.parseEither
        (Aeson.withObject "TextView" (.: "cborHex")) textView

sendFaucetFundsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> [(String, Coin)]
    -> IO ()
sendFaucetFundsTo tr conn dir targets = batch 80 targets $
    sendFaucet tr conn dir "ada" . map coinBundle
  where
    coinBundle = fmap (\c -> (TokenBundle.fromCoin c, []))

-- | Create transactions to fund the given faucet addresses with Ada and assets.
--
-- Beside the 'TokenBundle' of Ada and assets, there is a list of
-- @(signing key, verification key hash)@ pairs needed to sign the
-- minting transaction.
sendFaucetAssetsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> Int -- ^ batch size
    -> [(String, (TokenBundle, [(String, String)]))] -- ^ (address, assets)
    -> IO ()
sendFaucetAssetsTo tr conn dir batchSize targets = do
    era <- getClusterEra dir
    when (era >= MaryHardFork) $
        batch batchSize targets $ sendFaucet tr conn dir "assets"

-- | Build, sign, and send a batch of faucet funding transactions using
-- @cardano-cli@. This function is used by 'sendFaucetFundsTo' and
-- 'sendFaucetAssetsTo'.
sendFaucet
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> String -- ^ label for logging
    -> [(String, (TokenBundle, [(String, String)]))]
    -> IO ()
sendFaucet tr conn dir what targets = do
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "faucet-tx.raw"

    let mkOutput addr (TokenBundle (Coin c) tokens) =
            [ "--tx-out"
            , unwords $ [ addr, show c, "lovelace"] ++
                map (("+ " ++) . cliAsset) (TokenMap.toFlatList tokens)
            ]
        cliAsset (aid, (TokenQuantity q)) = unwords [show q, cliAssetId aid]
        cliAssetId (AssetId pid (UnsafeTokenName name)) = mconcat
            [ T.unpack (toText pid)
            , if B8.null name then "" else "."
            , B8.unpack (hex name)
            ]
        mkMint [] = []
        mkMint assets = ["--mint", intercalate " + " (map cliAsset assets)]

    let total = fromIntegral $ sum $
            map (unCoin . TokenBundle.getCoin . fst . snd) targets
    when (total > faucetAmt) $ error "sendFaucetFundsTo: too much to pay"

    let targetAssets = concatMap (snd . TokenBundle.toFlatList . fst . snd) targets

    scripts <- forM (nub $ concatMap (map snd . snd . snd) targets) $
        writeMonetaryPolicyScriptFile dir

    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "6000000"
            -- Big enough to allow minting in the actual integration tests,
            -- before the wallet API supports it.
        , "--fee", show (faucetAmt - total)
        , "--out-file", file
        ] ++
        concatMap (uncurry mkOutput . fmap fst) targets ++
        mkMint targetAssets ++
        (concatMap (\f -> ["--minting-script-file", f]) scripts)

    policyKeys <- forM (nub $ concatMap (snd . snd) targets) $
        \(skey, keyHash) -> writePolicySigningKey dir keyHash skey

    tx <- signTx tr dir file (faucetPrv:policyKeys)
    submitTx tr conn (what ++ " faucet tx") tx

batch :: Int -> [a] -> ([a] -> IO b) -> IO ()
batch s xs = forM_ (group s xs)
  where
    -- TODO: Use split package?
    -- https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take n l) : (group n (drop n l))
      | otherwise = error "Negative or zero n"

data Credential
    = KeyCredential XPub
    | ScriptCredential ByteString

moveInstantaneousRewardsTo
    :: Tracer IO ClusterLog
    -> CardanoNodeConn
    -> FilePath
    -> [(Credential, Coin)]
    -> IO ()
moveInstantaneousRewardsTo tr conn dir targets = do
    certs <- mapM mkCredentialCerts targets
    (faucetInput, faucetPrv) <- takeFaucet
    let file = dir </> "mir-tx.raw"

    let total = fromIntegral $ sum $ map (unCoin . snd) targets
    let totalDeposit = fromIntegral (length targets) * depositAmt
    when (total > faucetAmt) $ error "moveInstantaneousRewardsTo: too much to pay"

    sink <- genSinkAddress tr dir Nothing

    cli tr $
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--ttl", "999999999"
        , "--fee", show (faucetAmt - 1_000_000 - totalDeposit)
        , "--tx-out", sink <> "+" <> "1000000"
        , "--out-file", file
        ] ++ concatMap (\x -> ["--certificate-file", x]) (mconcat certs)

    testData <- getShelleyTestDataPath
    let bftPrv = testData </> "bft-leader" <> ".skey"

    tx <- signTx tr dir file [faucetPrv, bftPrv]
    submitTx tr conn "MIR certificates" tx
  where
    mkCredentialCerts
        :: (Credential, Coin)
        -> IO [FilePath]
    mkCredentialCerts = \case
        (KeyCredential xpub, coin) -> do
            (prefix, vkFile) <- mkVerificationKey xpub
            stakeAddr <- cliLine tr
                [ "stake-address"
                , "build"
                , "--mainnet"
                , "--stake-verification-key-file" , vkFile
                ]
            stakeCert <- issueStakeVkCert tr dir prefix vkFile
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [stakeCert, mirCert]

        (ScriptCredential script, coin) -> do
            (prefix, scriptFile) <- mkScript script
            -- NOTE: cardano-cli does not support creating stake-address from
            -- scripts just yet... So it's a bit ugly, but we create a stake
            -- address by creating a standard address, and replacing the header.
            stakeAddr <- toStakeAddress <$> cliLine tr
                [ "address"
                , "build"
                , "--mainnet"
                , "--payment-script-file" , scriptFile
                ]
            stakeCert <- issueStakeScriptCert tr dir prefix scriptFile
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [stakeCert, mirCert]

      where
        toStakeAddress =
            T.unpack
            . Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
            . BL.toStrict
            . BL.pack . mapFirst (240 .|.) . BL.unpack
            . unsafeBech32Decode
            . T.pack
          where
            hrp = [humanReadablePart|stake|]

    mkVerificationKey
        :: XPub
        -> IO (String, FilePath)
    mkVerificationKey xpub = do
        let base16 = T.unpack $ T.decodeUtf8 $ hex $ xpubPublicKey xpub
        let json = Aeson.object
                [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
                , "description" .= Aeson.String "Stake Verification Key"
                , "cborHex" .= Aeson.String ("5820" <> T.pack base16)
                ]
        let file = dir </> base16 <> ".vk"
        BL8.writeFile file (Aeson.encode json)
        pure (base16, file)

    mkScript
        :: ByteString
        -> IO (String, FilePath)
    mkScript bytes = do
        let base16 = T.decodeUtf8 $ hex $ CBOR.toStrictByteString $ CBOR.encodeBytes bytes
        let json = Aeson.object
                [ "type" .= Aeson.String "PlutusScriptV1"
                , "description" .= Aeson.String ""
                , "cborHex" .= Aeson.String base16
                ]
        let prefix = take 100 (T.unpack base16)
        let file = dir </> prefix <> ".plutus"
        BL8.writeFile file (Aeson.encode json)
        pure (prefix, file)

    mkMIRCertificate
        :: (String, Coin)
        -> IO FilePath
    mkMIRCertificate (stakeAddr, Coin reward) = do
        let mirCert = dir </> stakeAddr <> ".mir"
        cli tr
            [ "governance", "create-mir-certificate"
            , "--reserves"
            , "--reward", show reward
            , "--stake-address", stakeAddr
            , "--out-file", mirCert
            ]
        pure mirCert

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: Tracer IO ClusterLog
    -> FilePath
    -> IO (FilePath, FilePath)
prepareKeyRegistration tr dir = do
    let file = dir </> "tx.raw"

    let stakePub = dir </> "pre-registered-stake.pub"
    Aeson.encodeFile stakePub preRegisteredStakeKey

    (faucetInput, faucetPrv) <- takeFaucet

    cert <- issueStakeVkCert tr dir "pre-registered" stakePub
    sink <- genSinkAddress tr dir Nothing

    cli tr
        [ "transaction", "build-raw"
        , "--tx-in", faucetInput
        , "--tx-out", sink <> "+" <> "1000000"
        , "--ttl", "400"
        , "--fee", show (faucetAmt - depositAmt - 1_000_000)
        , "--certificate-file", cert
        , "--out-file", file
        ]
    pure (file, faucetPrv)

genSinkAddress
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Directory to put keys
    -> Maybe FilePath -- ^ Stake pub
    -> IO String
genSinkAddress tr dir stakePub = do
    let sinkPrv = dir </> "sink.prv"
    let sinkPub = dir </> "sink.pub"
    cli tr
        [ "address", "key-gen"
        , "--signing-key-file", sinkPrv
        , "--verification-key-file", sinkPub
        ]
    cliLine tr $
        [ "address", "build"
        , "--mainnet"
        , "--payment-verification-key-file", sinkPub
        ] ++ maybe [] (\key -> ["--stake-verification-key-file", key]) stakePub

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Tracer IO ClusterLog
    -> FilePath -- ^ Output directory
    -> FilePath -- ^ Tx body file
    -> [FilePath] -- ^ Signing keys for witnesses
    -> IO FilePath
signTx tr dir rawTx keys = do
    let file = dir </> "tx.signed"
    cli tr $
        [ "transaction", "sign"
        , "--tx-body-file", rawTx
        , "--mainnet"
        , "--out-file", file
        ]
        ++ concatMap (\key -> ["--signing-key-file", key]) keys
    pure file

-- | Submit a transaction through a running node.
submitTx :: Tracer IO ClusterLog -> CardanoNodeConn -> String -> FilePath -> IO ()
submitTx tr conn name signedTx =
    cliRetry tr ("Submitting transaction for " <> T.pack name) =<<
        cliConfigNode tr conn
        [ "transaction", "submit"
        , "--tx-file", signedTx
        , "--mainnet", "--cardano-mode"
        ]

-- | Hard-wired faucets referenced in the genesis file. Purpose is simply to
-- fund some initial transaction for the cluster. Faucet have plenty of money to
-- pay for certificates and are intended for a one-time usage in a single
-- transaction.
takeFaucet :: IO (String, String)
takeFaucet = do
    i <- modifyMVar faucetIndex (\i -> pure (i+1, i))
    source <- getShelleyTestDataPath
    let basename = source </> "faucet-addrs" </> "faucet" <> show i
    base58Addr <- BS.readFile $ basename <> ".addr"
    let addr = fromMaybe (error $ "decodeBase58 failed for " ++ show base58Addr)
            . decodeBase58 bitcoinAlphabet
            . T.encodeUtf8
            . T.strip
            $ T.decodeUtf8 base58Addr

    let txin = B8.unpack (hex $ blake2b256 addr) <> "#0"
    let signingKey = basename <> ".shelley.key"
    pure (txin, signingKey)

-- | List of faucets also referenced in the shelley 'genesis.yaml'
faucetIndex :: MVar Int
faucetIndex = unsafePerformIO $ newMVar 1
{-# NOINLINE faucetIndex #-}

-- | Allow running the test cluster a second time in the same process.
resetGlobals :: IO ()
resetGlobals = do
    void $ swapMVar faucetIndex 1

getClusterEra :: FilePath -> IO ClusterEra
getClusterEra dir = read <$> readFile (dir </> "era")

putClusterEra :: FilePath -> ClusterEra -> IO ()
putClusterEra dir = writeFile (dir </> "era") . show

-- | A public stake key associated with a mnemonic that we pre-registered for
-- STAKE_POOLS_JOIN_05.
--
-- ["over", "decorate", "flock", "badge", "beauty"
-- , "stamp", "chest", "owner", "excess", "omit"
-- , "bid", "raccoon", "spin", "reduce", "rival"
-- ]
preRegisteredStakeKey
    :: Aeson.Value
preRegisteredStakeKey = Aeson.object
    [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
    , "description" .= Aeson.String "Free form text"
    , "cborHex" .= Aeson.String
        "5820949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f"
    ]

-- | Deposit amount required for registering certificates.
depositAmt :: Integer
depositAmt = 1000000

-- | Initial amount in each of these special cluster faucet
faucetAmt :: Integer
faucetAmt = 1000 * oneMillionAda

-- | Just one million Ada, in Lovelace.
oneMillionAda :: Integer
oneMillionAda = 1_000_000_000_000

-- | Add a @setupScribes[1].scMinSev@ field in a given config object.
-- The full lens library would be quite helpful here.
addMinSeverityStdout
    :: MonadFail m
    => Severity
    -> Aeson.Object
    -> m Aeson.Object
addMinSeverityStdout severity ob = case Aeson.lookup "setupScribes" ob of
    Just (Aeson.Array scribes) -> do
        let scribes' = Aeson.Array $ fmap setMinSev scribes
        pure $ Aeson.insert "setupScribes" scribes' ob
    _ -> fail "setupScribes logging config is missing or the wrong type"
  where
    sev = toJSON $ show severity
    setMinSev (Aeson.Object scribe)
        | Aeson.lookup "scKind" scribe == Just (Aeson.String "StdoutSK")
            = Aeson.Object (Aeson.insert "scMinSev" sev scribe)
        | otherwise = Aeson.Object scribe
    setMinSev a = a

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject
    :: MonadFail m
    => (Aeson.Object -> m Aeson.Object)
    -> Aeson.Value
    -> m Aeson.Value
withObject action = \case
    Aeson.Object m -> Aeson.Object <$> action m
    _ -> fail
        "withObject: was given an invalid JSON. Expected an Object but got \
        \something else."

-- | Hash a ByteString using blake2b_256 and encode it in base16
blake2b256S :: ByteString -> String
blake2b256S =
    T.unpack
    . T.decodeUtf8
    . convertToBase Base16
    . blake2b256

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data ClusterLog
    = MsgRegisteringStakePools Int -- ^ How many pools
    | MsgStartingCluster FilePath
    | MsgLauncher String LauncherLog
    | MsgStartedStaticServer String FilePath
    | MsgRegisteringPoolMetadataInSMASH String String
    | MsgRegisteringPoolMetadata String String
    | MsgTempDir TempDirLog
    | MsgBracket Text BracketLog
    | MsgCLIStatus Text ExitCode BL8.ByteString BL8.ByteString
    | MsgCLIRetry Text
    | MsgCLIRetryResult Text Int BL8.ByteString
    | MsgSocketIsReady CardanoNodeConn
    | MsgStakeDistribution String ExitCode BL8.ByteString BL8.ByteString
    | MsgDebug Text
    | MsgGenOperatorKeyPair FilePath
    | MsgCLI [String]
    deriving (Show)

instance ToText ClusterLog where
    toText = \case
        MsgStartingCluster dir ->
            "Configuring cluster in " <> T.pack dir
        MsgRegisteringPoolMetadata url hash -> T.pack $ unwords
            [ "Hosting metadata for pool using url"
            , url
            , "with hash"
            , hash
            ]
        MsgRegisteringPoolMetadataInSMASH pool hash -> T.pack $ unwords
            [ "Registering metadata for pool"
            , pool
            , "with SMASH with the metadata hash"
            , hash
            ]
        MsgRegisteringStakePools n -> mconcat
                [ T.pack (show n)
                , " stake pools are being registered on chain... "
                ]
        MsgLauncher name msg ->
            T.pack name <> " " <> toText msg
        MsgStartedStaticServer baseUrl fp ->
            "Started a static server for " <> T.pack fp
                <> " at " <> T.pack baseUrl
        MsgTempDir msg -> toText msg
        MsgBracket name b -> name <> ": " <> toText b
        MsgCLIStatus msg st out err -> case st of
            ExitSuccess -> "Successfully finished " <> msg
            ExitFailure code -> "Failed " <> msg <> " with exit code " <>
                T.pack (show code)  <> ":\n" <> indent out <> "\n" <> indent err
        MsgCLIRetry msg -> msg
        MsgCLIRetryResult msg code err ->
            "Failed " <> msg <> " with exit code " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgSocketIsReady conn ->
            toText conn <> " is ready."
        MsgStakeDistribution name st out err -> case st of
            ExitSuccess ->
                "Stake distribution query for " <> T.pack name <>
                ":\n" <> indent out
            ExitFailure code ->
                "Query of stake-distribution failed with status " <>
                T.pack (show code) <> ":\n" <> indent err
        MsgDebug msg -> msg
        MsgGenOperatorKeyPair dir ->
            "Generating stake pool operator key pair in " <> T.pack dir
        MsgCLI args -> T.pack $ unwords ("cardano-cli":args)
      where
        indent = T.unlines . map ("  " <>) . T.lines . T.decodeUtf8With T.lenientDecode . BL8.toStrict

instance HasPrivacyAnnotation ClusterLog
instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        MsgStartingCluster _ -> Notice
        MsgRegisteringStakePools _ -> Notice
        MsgLauncher _ _ -> Info
        MsgStartedStaticServer _ _ -> Info
        MsgTempDir msg -> getSeverityAnnotation msg
        MsgBracket _ _ -> Debug
        MsgCLIStatus _ ExitSuccess _ _-> Debug
        MsgCLIStatus _ (ExitFailure _) _ _-> Error
        MsgCLIRetry _ -> Info
        MsgCLIRetryResult{} -> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgSocketIsReady _ -> Info
        MsgStakeDistribution _ ExitSuccess _ _-> Info
        MsgStakeDistribution _ (ExitFailure _) _ _-> Info
        -- NOTE: ^ Some failures are expected, so for cleaner logs we use Info,
        -- instead of Warning.
        MsgDebug _ -> Debug
        MsgGenOperatorKeyPair _ -> Debug
        MsgCLI _ -> Debug
        MsgRegisteringPoolMetadataInSMASH{} -> Info
        MsgRegisteringPoolMetadata{} -> Info

data TempDirLog = MsgNoCleanup FilePath BracketLog
    deriving stock (Show)

instance ToText TempDirLog where
    toText = \case
        MsgNoCleanup _ BracketStart -> ""
        MsgNoCleanup dir _ -> "NO_CLEANUP of temporary directory " <> T.pack dir

instance HasPrivacyAnnotation TempDirLog
instance HasSeverityAnnotation TempDirLog where
    getSeverityAnnotation = \case
        MsgNoCleanup _ BracketStart -> Debug
        MsgNoCleanup _ _ -> Notice

bracketTracer' :: Tracer IO ClusterLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)

-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port { getPort :: Int }
    deriving stock (Eq, Generic)
    deriving newtype (Enum, Ord, Show)

mkScribe :: LogOutput -> [ScribeDefinition]
mkScribe (LogToFile path sev) = pure $ ScribeDefinition
    { scName = T.pack path
    , scFormat = ScText
    , scKind = FileSK
    , scMinSev = sev
    , scMaxSev = Critical
    , scPrivacy = ScPublic
    , scRotation = Nothing
    }
mkScribe (LogToStdStreams sev) =
    [ mkScribe' (max errMin sev, maxBound, StderrSK)
    , mkScribe' (sev, pred errMin, StdoutSK)
    ]
  where
    errMin = Warning
    mkScribe' (minSev, maxSev, kind) = ScribeDefinition
        { scName = "text"
        , scFormat = ScText
        , scKind = kind
        , scMinSev = minSev
        , scMaxSev = maxSev
        , scPrivacy = ScPublic
        , scRotation = Nothing
        }

mkScribeId :: LogOutput -> [ScribeId]
mkScribeId (LogToStdStreams _) = ["StdoutSK::text", "StderrSK::text"]
mkScribeId (LogToFile file _) = pure $ T.pack $ "FileSK::" <> file

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer
    :: LoggerName
    -> [LogOutput]
    -> IO (Switchboard Text, (CM.Configuration, Trace IO Text))
initTracer loggerName outputs = do
    -- prometheusHP <- getPrometheusURL
    -- ekgHP <- getEKGURL
    cfg <- do
        c <- defaultConfigStdout
        CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK, CM.EKGViewBK, CM.EditorBK]
        CM.setDefaultBackends c [CM.KatipBK]
        CM.setSetupScribes c $ outputs >>= mkScribe
        CM.setDefaultScribes c $ outputs >>= mkScribeId
        CM.setBackends c "test-cluster.metrics" (Just [CM.EKGViewBK])
        CM.setBackends c "cardano-wallet.metrics" (Just [CM.EKGViewBK])
        -- forM_ ekgHP $ \(h, p) -> do
        --     CM.setEKGBindAddr c $ Just (Endpoint (h, getPort p))
        -- forM_ prometheusHP $ \(h, p) ->
        --     CM.setPrometheusBindAddr c $ Just (h, getPort p)
        pure c
    (tr, sb) <- setupTrace_ cfg loggerName
    -- TODO: see if we had it enabled anyway / if we want
    -- ekgEnabled >>= flip when (startCapturingMetrics tr)
    pure (sb, (cfg, tr))
  where
    -- -- https://github.com/input-output-hk/cardano-node/blob/f7d57e30c47028ba2aeb306a4f21b47bb41dec01/cardano-node/src/Cardano/Node/Configuration/Logging.hs#L224
    -- startCapturingMetrics :: Trace IO Text -> IO ()
    -- startCapturingMetrics trace0 = do
    --   let trace = appendName "metrics" trace0
    --       counters = [Obs.MemoryStats, Obs.ProcessStats
    --         , Obs.NetStats, Obs.IOStats, Obs.GhcRtsStats, Obs.SysStats]
    --   _ <- Async.async $ forever $ do
    --     cts <- readCounters (ObservableTraceSelf counters)
    --     traceCounters trace cts
    --     threadDelay 30_000_000   -- 30 seconds
    --   pure ()
    --  where
    --    traceCounters :: forall m a. MonadIO m => Trace m a -> [Counter] -> m ()
    --    traceCounters _tr [] = return ()
    --    traceCounters tr (c@(Counter _ct cn cv) : cs) = do
    --      mle <- mkLOMeta Notice Confidential
    --      traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
    --      traceCounters tr cs

-- | Run an action with logging available and configured. When the action is
-- finished (normally or otherwise), log messages are flushed.
withLoggingNamed
    :: LoggerName
    -> [LogOutput]
    -> ((Switchboard Text, (CM.Configuration, Trace IO Text)) -> IO a)
    -- ^ The action to run with logging configured.
    -> IO a
withLoggingNamed loggerName outputs = bracket before after
  where
    before = initTracer loggerName outputs
    after (sb, (_, tr)) = do
        logDebug (appendName "main" tr) "Logging shutdown."
        shutdown sb

{-------------------------------------------------------------------------------
                           Environment Variable Utils
-------------------------------------------------------------------------------}

-- | Looks up an environment variable, treating variables which are defined but
-- empty the same as variables which are undefined.
lookupEnvNonEmpty :: MonadUnliftIO m => String -> m (Maybe String)
lookupEnvNonEmpty = liftIO . fmap nonEmpty . lookupEnv
  where
    nonEmpty (Just "") = Nothing
    nonEmpty m = m

-- | Returns true iff an environment variable is defined and non-empty.
isEnvSet :: MonadUnliftIO m => String -> m Bool
isEnvSet = fmap isJust . lookupEnvNonEmpty

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Create a temporary directory and remove it after the given IO action has
-- finished -- unless the @NO_CLEANUP@ environment variable has been set.
withTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> FilePath -- ^ Parent directory
    -> String -- ^ Directory name template
    -> (FilePath -> m a) -- ^ Callback that can use the directory
    -> m a
withTempDir tr parent name action = isEnvSet "NO_CLEANUP" >>= \case
    True -> do
        dir <- liftIO $ createTempDirectory parent name
        let tr' = contramap (MsgNoCleanup dir) tr
        bracketTracer tr' $ action dir
    False -> withTempDirectory parent name action

withSystemTempDir
    :: MonadUnliftIO m
    => Tracer m TempDirLog
    -> String   -- ^ Directory name template
    -> (FilePath -> m a) -- ^ Callback that can use the directory
    -> m a
withSystemTempDir tr name action = do
    parent <- liftIO getCanonicalTemporaryDirectory
    withTempDir tr parent name action


{-------------------------------------------------------------------------------
                           Orphan Instances
-------------------------------------------------------------------------------}

instance ToJSON SMASHPoolId where
    toJSON = genericToJSON defaultRecordTypeOptions
        { fieldLabelModifier = Prelude.id }

instance ToJSON HealthStatusSMASH where
    toJSON = genericToJSON defaultRecordTypeOptions

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
    , omitNothingFields = True
    }