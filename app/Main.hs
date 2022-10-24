{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Ado Bot modules
import Config.BotConfig             (botConfig)
import Config.Type                  (BotConfig (..))
import DiscordBot.Events            (onDiscordEvent)
import DiscordBot.Events.BotStarted (onBotStarted)
import DiscordBot.Guilds.Settings   (getSettingsDb)
import Notifications.History        (getNotifHistoryDb)

-- Downloaded libraries
import Discord       (runDiscord, def, RunDiscordOpts (..))
import Discord.Types (GatewayIntent (..))

------------------------------------------------------------------------------

main :: IO ()
main = do
  echo "Application started."

  notifHistoryDb <- getNotifHistoryDb
  settingsDb     <- getSettingsDb

  botTerminationError <- runDiscord $ def
    { discordToken         = botToken botConfig
    , discordOnEvent       = onDiscordEvent settingsDb
    , discordOnStart       = onBotStarted settingsDb notifHistoryDb
    , discordGatewayIntent = def { gatewayIntentMessageContent = False }
    }

  echo $ "A fatal error occurred: " <> botTerminationError
