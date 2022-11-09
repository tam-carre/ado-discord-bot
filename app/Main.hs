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
import App.Types (Db(Db))

------------------------------------------------------------------------------

main :: IO ()
main = do
  echo "Application started."

  db <- Db <$> getNotifHistoryDb <*> getSettingsDb

  botTerminationError <-
    runDiscord $ def & token   .~ botConfig^.botToken
                     & onEvent .~ usingReaderT db . onDiscordEvent
                     & onStart .~ runReaderT onBotStarted db
                     & gatewayIntent .~ (def & messageContent .~ False)

  echo $ "A fatal error occurred: " <> botTerminationError
