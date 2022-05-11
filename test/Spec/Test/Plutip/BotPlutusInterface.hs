module Spec.Test.Plutip.BotPlutusInterface (test) where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Network.HTTP.Client (
  Response (responseStatus),
  defaultManagerSettings,
  httpNoBody,
  newManager,
  parseRequest,
 )
import Network.HTTP.Types.Status (status200)
import System.Directory (doesDirectoryExist, doesFileExist)
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir, pParamsFile)
import Test.Plutip.Internal.LocalCluster (withPlutusInterface)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

test :: TestTree
test = testCase "Bot interface integration" $ do
  withPlutusInterface def [] $ \cEnv -> do
    liftIO $
      doesDirectoryExist (keysDir cEnv)
        >>= assertBool "Required directory not found after setup run"
    liftIO $
      doesFileExist (pParamsFile cEnv)
        >>= assertBool "Protocol params file not found after setup run"
    liftIO $
      tipChainIndex
        >>= assertBool "Chain index not available" . (== status200) . responseStatus
  where
    tipChainIndex = do
      mgr <- newManager defaultManagerSettings
      req <- parseRequest "http://localhost:9083/tip" --TODO: port from cEnv
      httpNoBody req mgr
