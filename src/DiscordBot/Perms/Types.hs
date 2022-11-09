module DiscordBot.Perms.Types (PermLvl (..)) where

data PermLvl
  = PermLvlUser
  | PermLvlBotManager
  | PermLvlBotOwner
  deriving (Eq, Ord, Show)

