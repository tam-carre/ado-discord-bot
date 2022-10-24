{-# LANGUAGE OverloadedStrings #-}

module Config.BotConfig (botConfig) where

-- Ado Bot modules
import Config.Type   (BotConfig (..))
import Config.Tokens (discordProd, deepl) -- not version controlled obv

-------------------------------------------------------------------------------

botConfig :: BotConfig
botConfig = BotConfig
  { botToken = discordProd
  , ownerDebugChannelId = 1031895573218983968
  , ownerUserId = 150696503428644864
  , deeplKey = deepl
  , inviteUrl = "https://discordapp.com/oauth2/authorize?client_id=1031887395467046923&scope=bot&permissions=517543873600"
  }
