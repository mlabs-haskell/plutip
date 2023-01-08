module Main (main) where

import Spec.ClusterStartup qualified as ClusterStartup

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ ClusterStartup.test
      ]
