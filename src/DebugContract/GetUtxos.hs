module DebugContract.GetUtxos (getUtxos, getUtxosThrowsErr, getUtxosThrowsEx) where

import Data.Map (Map)
import Data.Text (Text, pack)
import Ledger (ChainIndexTxOut, TxOutRef, pubKeyHashAddress)
import Plutus.Contract (Contract, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Text.Printf (printf)

getUtxos :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxosAt $ pubKeyHashAddress pkh Nothing

getUtxosThrowsErr :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
  Contract.throwError (mkMsg utxos)
  where
    mkMsg utxos =
      pack $
        mconcat
          [ "This Error was thrown intentionally by Contract \n"
          , "Debug: Own UTxOs: " <> show utxos <> "\n"
          ]

getUtxosThrowsEx :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsEx = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
  error $
    mconcat
      [ "This Exception was thrown intentionally in Contract.\n"
      , "Debug: Own UTxOs: " <> show utxos <> "\n"
      ]
