module Main (main) where

import Spec.Integration qualified as Integration
import Spec.Test.Plutip.BotPlutusInterface qualified as BotInterface
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      -- FIXME: both `Integration.test` and `BotInterface.test`
      -- start own cluster to run tests, probably, need better solution in future
      [  Integration.test
      --,
      --  Integration.test
      -- , BotInterface.test
      ]
