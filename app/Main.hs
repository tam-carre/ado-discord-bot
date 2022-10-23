{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Ado Bot modules
import Config.BotConfig             (botConfig)
import Config.Type                  (BotConfig (..))
import DiscordBot.Events            (onDiscordEvent)
import DiscordBot.Events.BotStarted (onBotStarted)
import DiscordBot.Guilds.Settings   (getSettingsDb, SettingsDb)

-- Downloaded libraries
import Discord       (runDiscord, def, RunDiscordOpts (..))
import Discord.Types (GatewayIntent (..))
import Data.Acid     (AcidState)

------------------------------------------------------------------------------

main :: IO ()
main = do
  echo "Application started."
  settingsDb          <- getSettingsDb
  botTerminationError <- runDiscord $ options settingsDb
  echo $ "A fatal error occurred: " <> botTerminationError

options :: AcidState SettingsDb -> RunDiscordOpts
options settingsDb = def
  { discordToken         = botToken botConfig
  , discordOnEvent       = onDiscordEvent settingsDb
  , discordOnStart       = onBotStarted
  , discordGatewayIntent = def { gatewayIntentMessageContent = False }
  }
