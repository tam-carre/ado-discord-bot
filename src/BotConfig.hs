{-# LANGUAGE OverloadedStrings #-}

module BotConfig (botConfig, BotConfig (..)) where

-- Ado Bot modules
import qualified Tokens -- not version controlled obv

-------------------------------------------------------------------------------

data BotConfig = BotConfig
  { botToken            :: Text
  , deeplKey            :: Text
  , ownerDebugChannelId :: Word64
  , ownerUserId         :: Word64
  , inviteUrl           :: Text
  }

botConfig :: BotConfig
botConfig = BotConfig
  { botToken = Tokens.discord
  , ownerDebugChannelId = 1031895573218983968
  , ownerUserId = 150696503428644864
  , deeplKey = Tokens.deepl
  , inviteUrl = "https://discordapp.com/oauth2/authorize?client_id=1031887395467046923&scope=bot&permissions=517543873600"
  }
