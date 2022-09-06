module Test.Plutip.Internal.BotPlutusInterface.Keys (
  KeyPair (sKey, vKey),
  StakeKeyPair (sSKey, sVKey),
  genKeyPair,
  genKeyPairs,
  genStakeKeyPair,
  genStakeKeyPairs,
  genWalletKeys,
  genWriteWalletKeys,
) where

import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api qualified as CAPI
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Crypto qualified as Shelley
import Data.Proxy (Proxy (Proxy))
import System.FilePath ((<.>), (</>))

data KeyPair = KeyPair
  { sKey :: CAPI.SigningKey CAPI.PaymentKey
  , vKey :: CAPI.VerificationKey CAPI.PaymentKey
  }
  deriving stock (Show)

data StakeKeyPair = StakeKeyPair
  { sSKey :: CAPI.SigningKey CAPI.StakeKey
  , sVKey :: CAPI.VerificationKey CAPI.StakeKey
  }
  deriving stock (Show)

genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- CAPI.generateSigningKey CAPI.AsPaymentKey
  return $ KeyPair sKey (CAPI.getVerificationKey sKey)

-- | Helper to generate key pairs.
-- Can be further developed to generate test keys for test wallets
-- to work with `bot-plutus-interface`
-- >>> genKeyPairs "cluster-data/known_wallets" "signing-key-" "verification-key-"
genKeyPairs :: FilePath -> String -> String -> IO ()
genKeyPairs outDir sKeyPrefix vKeyPrefix = do
  kp <- genKeyPair
  let skeyDesc, vkeyDesc :: CAPI.TextEnvelopeDescr
      skeyDesc = "Payment Signing Key"
      vkeyDesc = "Payment Verification Key"

      hash = CAPI.verificationKeyHash $ vKey kp

      skeyPath = rmQuotes $ outDir </> sKeyPrefix ++ showHash hash <.> "skey"
      vkeyPath = rmQuotes $ outDir </> vKeyPrefix ++ showHash hash <.> "vkey"

      showHash = rmQuotes . show
  res <-
    sequence
      [ writeFileTextEnvelope skeyPath (Just skeyDesc) (sKey kp)
      , writeFileTextEnvelope vkeyPath (Just vkeyDesc) (vKey kp)
      ]
  print res

rmQuotes :: String -> String
rmQuotes = filter (/= '"')

genStakeKeyPair :: IO StakeKeyPair
genStakeKeyPair = do
  sKey <- CAPI.generateSigningKey CAPI.AsStakeKey
  return $ StakeKeyPair sKey (CAPI.getVerificationKey sKey)

genStakeKeyPairs :: FilePath -> String -> String -> IO ()
genStakeKeyPairs outDir sKeyPrefix vKeyPrefix = do
  skp <- genStakeKeyPair
  let sSKeyDesc, sVKeyDesc :: CAPI.TextEnvelopeDescr
      sSKeyDesc = "Delegation Signing Key"
      sVKeyDesc = "Delegation Verification Key"

      hash = CAPI.verificationKeyHash $ sVKey skp

      skeyPath = rmQuotes $ outDir </> sKeyPrefix ++ showHash hash <.> "skey"
      vkeyPath = rmQuotes $ outDir </> vKeyPrefix ++ showHash hash <.> "vkey"

      showHash = rmQuotes . show
  res <-
    sequence
      [ writeFileTextEnvelope skeyPath (Just sSKeyDesc) (sSKey skp)
      , writeFileTextEnvelope vkeyPath (Just sVKeyDesc) (sVKey skp)
      ]
  print res

genWalletKeys :: IO (KeyPair, StakeKeyPair)
genWalletKeys = do
  seed <- Crypto.readSeedFromSystemEntropy $ Crypto.seedSizeDSIGN proxy
  let sKey = CAPI.deterministicSigningKey CAPI.AsPaymentKey seed
      sSKey = CAPI.deterministicSigningKey CAPI.AsStakeKey seed
      kp = KeyPair sKey (CAPI.getVerificationKey sKey)
      skp = StakeKeyPair sSKey (CAPI.getVerificationKey sSKey)
  return (kp, skp)
  where
    proxy :: Proxy (Shelley.DSIGN StandardCrypto)
    proxy = Proxy

genWriteWalletKeys :: FilePath -> String -> String -> String -> String -> IO ()
genWriteWalletKeys outDir pSKeyPrefix pVKeyPrefix sSKeyPrefix sVKeyPrefix = do
  (pkp, skp) <- genWalletKeys
  let pSKeyDesc, pVKeyDesc, sSKeyDesc, sVKeyDesc :: CAPI.TextEnvelopeDescr
      pSKeyDesc = "Payment Signing Key"
      pVKeyDesc = "Payment Verification Key"
      sSKeyDesc = "Delegation Signing Key"
      sVKeyDesc = "Delegation Verification Key"

      pHash = rmQuotes . show $ CAPI.verificationKeyHash $ vKey pkp
      sHash = rmQuotes . show $ CAPI.verificationKeyHash $ sVKey skp

      pSKeyPath = rmQuotes $ outDir </> pSKeyPrefix ++ pHash <.> "skey"
      pVKeyPath = rmQuotes $ outDir </> pVKeyPrefix ++ pHash <.> "vkey"
      sSKeyPath = rmQuotes $ outDir </> sSKeyPrefix ++ sHash <.> "skey"
      sVKeyPath = rmQuotes $ outDir </> sVKeyPrefix ++ sHash <.> "vkey"
  res <-
    sequence
      [ writeFileTextEnvelope pSKeyPath (Just pSKeyDesc) (sKey pkp)
      , writeFileTextEnvelope pVKeyPath (Just pVKeyDesc) (vKey pkp)
      , writeFileTextEnvelope sSKeyPath (Just sSKeyDesc) (sSKey skp)
      , writeFileTextEnvelope sVKeyPath (Just sVKeyDesc) (sVKey skp)
      ]
  print res
