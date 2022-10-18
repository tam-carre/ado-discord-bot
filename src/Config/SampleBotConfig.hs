-- | To run the bot, fill out the config, and change the filename
-- and the module name (line 6) from SampleBotConfig to BotConfig

{-# LANGUAGE OverloadedStrings #-}

module Config.SampleBotConfig (botConfig) where

-- Ado Bot modules
import Config.Type (BotConfig (..))

-------------------------------------------------------------------------------

botConfig :: BotConfig
botConfig = BotConfig
  { botToken = "42"
  , ownerDebugChannelId = 42
  , ownerUserId = 42
  , deeplKey = "42"
  , inviteUrl = "https://discordapp.com/oauth2/authorize?client_id=<CLIENT_ID>&scope=bot&permissions=<PERMISSIONS>"
  }
