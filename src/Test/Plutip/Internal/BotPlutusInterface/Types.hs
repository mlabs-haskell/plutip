module Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiError (..),
) where

data BpiError
  = SignKeySaveError !String
  | BotInterfaceDirMissing
  deriving stock (Show)
