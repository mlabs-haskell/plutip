{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- Warnings turned off intetnionally to keep module close to the original
-- as much as possible for easier maintenance.
{-# OPTIONS_GHC -Wwarn=missing-import-lists #-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wwarn=missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wwarn=name-shadowing #-}
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

module Plutip.Launch.PoolConfigs
    ( defaultPoolConfigs
    , PoolRecipe(..) ) where

import Cardano.Wallet.Primitive.Types
    ( EpochNo
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Data.Aeson
    ( (.=) )

import Cardano.Pool.Types
    ( PoolId (PoolId) )
import qualified Data.Aeson as Aeson

-- | The idea of what kind if pool we want to set up.
data PoolRecipe = PoolRecipe
    { pledgeAmt :: Integer
    , index :: Int
    , retirementEpoch :: Maybe EpochNo
      -- ^ An optional retirement epoch. If specified, then a pool retirement
      -- certificate will be published after the pool is initially registered.
    , poolMetadata :: Aeson.Value
    , operatorKeys :: (PoolId, Aeson.Value, Aeson.Value, Aeson.Value)
      -- ^ @(poolId, vk, sk, counter)@ - as long as the integration tests make
      -- use of hard-coded pool ids, we need to pre-assign the operator keys and
      -- related data already here.
    , delisted :: Bool
      -- ^ Tells @withSMASH@ whether to delist this pool or not. Aside from
      -- this, a delisted pool will operate as normal.
    }
    deriving (Eq, Show)


defaultPoolConfigs :: [PoolRecipe]
defaultPoolConfigs = zipWith (\i p -> p {index = i}) [1..]
    [ -- This pool should never retire:
      PoolRecipe
        { pledgeAmt = 200 * millionAda
        , retirementEpoch = Nothing
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool A"
              , "ticker" .= Aeson.String "GPA"
              , "description" .= Aeson.Null
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = False
        , operatorKeys =
            ( PoolId $ unsafeFromHex
                "ec28f33dcbe6d6400a1e5e339bd0647c0973ca6c0cf9c2bbe6838dc6"
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820d8f81c455ef786f47ad9f573e49dc417e0125dfa8db986d6c0ddc03be8634dc6"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                , "description" .= Aeson.String "Next certificate issue number: 0"
                , "cborHex" .= Aeson.String
                    "82005820a12804d805eff46c691da5b11eb703cbf7463983e325621b41ac5b24e4b51887"
                ]
              )

        , index = undefined
        }
    , PoolRecipe
        { pledgeAmt = 100 * millionAda
        , retirementEpoch = Nothing
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool B"
              , "ticker" .= Aeson.String "GPB"
              , "description" .= Aeson.Null
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = False
    , operatorKeys =
          ( PoolId $ unsafeFromHex
              "1b3dc19c6ab89eaffc8501f375bb03c11bf8ed5d183736b1d80413d6"
          , Aeson.object
              [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
              , "description" .= Aeson.String "Stake pool operator key"
              , "cborHex" .= Aeson.String
                  "5820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
              ]
          , Aeson.object
              [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
              , "description" .= Aeson.String "Stake pool operator key"
              , "cborHex" .= Aeson.String
                  "5820fab9d94c52b3e222ed494f84020a29ef8405228d509a924106d05ed01c923547"
              ]
          , Aeson.object
              [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
              , "description" .= Aeson.String "Next certificate issue number: 0"
              , "cborHex" .= Aeson.String
                  "82005820109440baecebefd92e3b933b4a717dae8d3291edee85f27ebac1f40f945ad9d4"
              ]
          )
        , index = undefined
        }

    , PoolRecipe
        { pledgeAmt = 100 * millionAda
        , retirementEpoch = Nothing
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool C"
              , "ticker" .= Aeson.String "GPC"
              , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = True
        , operatorKeys =
            ( PoolId $ unsafeFromHex
                "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2"
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                , "description" .= Aeson.String "Stake pool operator key"
                , "cborHex" .= Aeson.String
                    "5820047572e48be93834d6d7ddb01bb1ad889b4de5a7a1a78112f1edd46284250869"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                , "description" .= Aeson.String "Next certificate issue number: 0"
                , "cborHex" .= Aeson.String
                    "82005820c7383d89aa33656464a7796b06616c4590d6db018b2f73640be985794db0702d"
                ]
            )
        , index = undefined
        }
    , PoolRecipe
        { pledgeAmt = 100 * millionAda
        , retirementEpoch = Nothing
        , poolMetadata = Aeson.object
              [ "name" .= Aeson.String "Genesis Pool D"
              , "ticker" .= Aeson.String "GPD"
              , "description" .= Aeson.String "Lorem Ipsum Dolor Sit Amet."
              , "homepage" .= Aeson.String "https://iohk.io"
              ]
        , delisted = False
        , operatorKeys =
            ( PoolId $ unsafeFromHex
                "bb114cb37d75fa05260328c235a3dae295a33d0ba674a5eb1e3e568e"
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolVerificationKey_ed25519"
                , "description" .= Aeson.String "Stake Pool Operator Verification Key"
                , "cborHex" .= Aeson.String
                    "58203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "StakePoolSigningKey_ed25519"
                , "description" .= Aeson.String "Stake Pool Operator Signing Key"
                , "cborHex" .= Aeson.String
                    "58208f50de27d74325eaf57767d70277210b31eb97cdc3033f632a9791a3677a64d2"
                ]
            , Aeson.object
                [ "type" .= Aeson.String "NodeOperationalCertificateIssueCounter"
                , "description" .= Aeson.String "Next certificate issue number: 0"
                , "cborHex" .= Aeson.String
                    "820058203263e07605b9fc0100eb520d317f472ae12989fbf27fc71f46310bc0f24f2970"
                ]
            )
        , index = undefined
        }
    ]
  where
    millionAda = 1_000_000_000_000
