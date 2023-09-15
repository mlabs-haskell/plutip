module Plutip.DistributeFunds (Lovelace (Lovelace), fundKey) where

import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))

import Plutip.Launch.Cluster (
  sendFaucetFundsTo,
 )

import Plutip.Types (ClusterEnv (clusterEra, supportDir), nodeSocket)

import Cardano.Api (Lovelace (Lovelace))
import Plutip.Keys (KeyPair, mainnetAddress)

-- | Fund key's enterprise address with a transaction spending faucet funds.
fundKey :: ClusterEnv -> KeyPair -> [Lovelace] -> IO ()
fundKey cEnv keys funds = do
  let fundAddress = mainnetAddress keys
      toAmt = Coin . fromIntegral
  sendFaucetFundsTo
    nullTracer -- todo: fix tracer to be not `nullTracer`
    (nodeSocket cEnv)
    (supportDir cEnv)
    (clusterEra cEnv)
    [(fundAddress, toAmt v) | v <- funds]
