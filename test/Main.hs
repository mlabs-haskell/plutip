module Main (main) where

import Test.BotInterface qualified as BotInterface
import Test.Integration qualified as Integration
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      -- fixme: both `Integration.test` and `BotInterface.test`
      -- start own cluster to run tests, need better solution in future
      [ Integration.test
      , BotInterface.test
      ]
