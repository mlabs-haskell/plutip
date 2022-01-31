module Keys (genKeyPairs) where

import Cardano.Api (AsType (AsPaymentKey), Key (getVerificationKey, verificationKeyHash), TextEnvelopeDescr, generateSigningKey, writeFileTextEnvelope)
import System.FilePath ((<.>), (</>))

-- | Helper to generate key pairs.
-- Can be further developed to generate test keys for test wallets
-- to work with `bot-plutus-interface`
-- >>> genKeyPairs "cluster-data/known_wallets" "signing-key-" "verification-key-"
genKeyPairs :: FilePath -> String -> String -> IO ()
genKeyPairs outDir sKeyPrefix vKeyPrefix = do
  sKey <- generateSigningKey AsPaymentKey
  let skeyDesc, vkeyDesc :: TextEnvelopeDescr
      skeyDesc = "Payment Signing Key"
      vkeyDesc = "Payment Verification Key"

      vKey = getVerificationKey sKey
      hash = verificationKeyHash vKey

      skeyPath = rmQuotes $ outDir </> sKeyPrefix ++ showHash hash <.> "skey"
      vkeyPath = rmQuotes $ outDir </> vKeyPrefix ++ showHash hash <.> "skey"

      showHash = rmQuotes . show
  res <-
    sequence
      [ writeFileTextEnvelope skeyPath (Just skeyDesc) sKey,
        writeFileTextEnvelope vkeyPath (Just vkeyDesc) vKey
      ]
  print res

rmQuotes = filter (/= '"')