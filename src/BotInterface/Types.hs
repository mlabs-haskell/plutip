module BotInterface.Types (
  BpiError (..),
) where

data BpiError
  = SignKeySaveError String
  | BotInterfaceDirMissing
  deriving stock (Show)
