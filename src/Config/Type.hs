module Config.Type (BotConfig (..)) where

-------------------------------------------------------------------------------

data BotConfig = BotConfig
  { botToken            :: Text
  , deeplKey            :: Text
  , ownerDebugChannelId :: Word64
  , ownerUserId         :: Word64
  , inviteUrl           :: Text
  }
