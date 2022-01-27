module Main (main) where

import Test.Integration qualified as Integration
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ Integration.test
      ]
