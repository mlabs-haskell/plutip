-- |For prototyping
module LocalCluster.Action where

import LocalCluster.Types
import LocalCluster.Wallet
import Data.Set qualified as Set
import LocalCluster.CardanoApi
import Cardano.Api qualified as C


import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (datumHash)
import Ledger.TimeSlot (posixTimeToEnclosingSlot, slotToEndPOSIXTime)
import Ledger.Typed.Scripts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval ()
import Plutus.V1.Ledger.Slot
import Plutus.V1.Ledger.Tx
import Plutus.V1.Ledger.Value
import qualified Plutus.V1.Ledger.Ada as Value
import LocalCluster.Wallet (paymentPubKeyHash)
import Data.Map qualified as Map
import Ledger.Tx.CardanoAPI (fromCardanoTxOut, fromCardanoAddress, FromCardanoError, fromCardanoTxIn)
import Control.Arrow (left)
import System.Exit (die)

{-
-- | A transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs      :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txCollateral  :: Set.Set TxIn,
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs     :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint        :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee         :: !Value,
    -- ^ The fee for this transaction.
    txValidRange  :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintScripts :: Set.Set MintingPolicy,
    -- ^ The scripts that must be run to check minting conditions.
    txSignatures  :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txRedeemers   :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    txData        :: Map DatumHash Datum
    -- ^ Datum objects recorded on this transaction.
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise, NFData)
-}

-- | For debug: takes single input from `from` and sends whole value to `to`
debugPayWalletToWallet :: ClusterEnv -> ClusterWallet -> ClusterWallet -> IO ()
debugPayWalletToWallet cEnv wFrom wTo = do
  cardanoUtxos <- utxosAtAddress cEnv (cwPaymentAddress wFrom)
  let txOuts = left show cardanoUtxos >>= left show . toPlutus -- FIXME: show error shortcut
  case txOuts of
    Right [(oref, out)] -> undefined
    other -> die $ "Ouch outs: " <> show other
  where
    -- todo: finish buildin Tx, then sign and submit
    ref = undefined
    toPkh = paymentPubKeyHash wTo
    val = undefined
    fee = Value.adaValueOf 2

    tx =
      mempty { txInputs = Set.singleton $ TxIn ref (Just ConsumePublicKeyAddress)}
      <> mempty { txOutputs = [TxOut (pubKeyHashAddress toPkh) val Nothing]}
      <> mempty { txFee = fee}

-- toPlutus :: C.UTxO C.AlonzoEra -> Int
toPlutus :: C.UTxO C.AlonzoEra -> Either FromCardanoError [(TxOutRef, TxOut)]
toPlutus cUtxo= v
  where
    v = mapM (\(ti, to) -> (fromCardanoTxIn ti, ) <$> fromCardanoTxOut (wtf to)  )
        $ Map.toList (C.unUTxO cUtxo)
    wtf :: C.TxOut C.CtxUTxO C.AlonzoEra -> C.TxOut C.CtxTx C.AlonzoEra
    wtf (C.TxOut a b C.TxOutDatumNone)= C.TxOut a b C.TxOutDatumNone
    wtf _ = error "wtf conversion failed" -- FIXME



-- fromCardanoTxOut' (C.TxOut addr value datumHash) =
--     TxOut
--     <$> fromCardanoAddress addr
--     <*> pure (fromCardanoTxOutValue value)
--     <*> pure (fromCardanoTxOutDatumHash datumHash)