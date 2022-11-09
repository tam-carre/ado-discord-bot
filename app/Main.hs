{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Ado Bot modules
import Lenses
import BotConfig                    (botConfig)
import DiscordBot.Events            (onDiscordEvent)
import DiscordBot.Events.BotStarted (onBotStarted)
import DiscordBot.Guilds.Settings   (getSettingsDb)
import Notifications.History        (getNotifHistoryDb)

-- Downloaded libraries
import Discord (runDiscord, def)

------------------------------------------------------------------------------

main :: IO ()
main = do
  echo "Application started."

  notifHistoryDb <- getNotifHistoryDb
  settingsDb     <- getSettingsDb

  botTerminationError <- runDiscord $ def
    & token         .~ botConfig^.botToken
    & onEvent       .~ onDiscordEvent settingsDb
    & onStart       .~ onBotStarted settingsDb notifHistoryDb
    & gatewayIntent .~ (def & messageContent .~ False)

  echo $ "A fatal error occurred: " <> botTerminationError
