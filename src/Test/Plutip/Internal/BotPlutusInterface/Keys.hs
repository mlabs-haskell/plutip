module Test.Plutip.Internal.BotPlutusInterface.Keys (
  KeyPair (sKey, vKey),
  StakeKeyPair (sSKey, sVKey),
  genKeyPair,
  writeKeyPair,
  genStakeKeyPair,
  writeStakeKeyPairs,
) where

import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api qualified as CAPI
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

rmQuotes :: String -> String
rmQuotes = filter (/= '"')

pSKeyDesc, pVKeyDesc, sSKeyDesc, sVKeyDesc :: CAPI.TextEnvelopeDescr
pSKeyDesc = "Payment Signing Key"
pVKeyDesc = "Payment Verification Key"
sSKeyDesc = "Delegation Signing Key"
sVKeyDesc = "Delegation Verification Key"

-- | Helper to generate key pairs.
-- Can be further developed to generate test keys for test wallets
-- to work with `bot-plutus-interface`
genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- CAPI.generateSigningKey CAPI.AsPaymentKey
  return $ KeyPair sKey (CAPI.getVerificationKey sKey)

writeKeyPair :: FilePath -> KeyPair -> IO [Either (CAPI.FileError ()) ()]
writeKeyPair outDir keyPair = do
  let hash = rmQuotes . show $ CAPI.verificationKeyHash $ vKey keyPair

      skeyPath = rmQuotes $ outDir </> "payment-signing-key-" ++ hash <.> "skey"
      vkeyPath = rmQuotes $ outDir </> "payment-verification-key-" ++ hash <.> "vkey"

  sequence
    [ writeFileTextEnvelope skeyPath (Just pSKeyDesc) (sKey keyPair)
    , writeFileTextEnvelope vkeyPath (Just pVKeyDesc) (vKey keyPair)
    ]

genStakeKeyPair :: IO StakeKeyPair
genStakeKeyPair = do
  sKey <- CAPI.generateSigningKey CAPI.AsStakeKey
  return $ StakeKeyPair sKey (CAPI.getVerificationKey sKey)

writeStakeKeyPairs :: FilePath -> StakeKeyPair -> IO [Either (CAPI.FileError ()) ()]
writeStakeKeyPairs outDir stakeKeyPair = do
  let hash = rmQuotes . show $ CAPI.verificationKeyHash $ sVKey stakeKeyPair

      skeyPath = rmQuotes $ outDir </> "delegation-signing-key-" ++ hash <.> "skey"
      vkeyPath = rmQuotes $ outDir </> "delegation-verifiaction-key-" ++ hash <.> "vkey"

  sequence
    [ writeFileTextEnvelope skeyPath (Just sSKeyDesc) (sSKey stakeKeyPair)
    , writeFileTextEnvelope vkeyPath (Just sVKeyDesc) (sVKey stakeKeyPair)
    ]
