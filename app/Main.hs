{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Ado Bot modules
import Lenses
import App                                        (Env (Env))
import BotConfig                                  (botConfig)
import DiscordBot.Events                          (onDiscordEvent)
import DiscordBot.Events.BotStartedStartNotifiers (onBotStartedStartNotifiers)
import DiscordBot.Guilds.Settings                 (getSettingsDb)
import Notifications.History                      (getNotifHistoryDb)

-- Downloaded libraries
import Discord (runDiscord, def)

------------------------------------------------------------------------------

main :: IO ()
main = do
  echo "Bot started."

  withEnv <- usingReaderT <$> (Env <$> getNotifHistoryDb <*> getSettingsDb)

  botTerminationError <-
    runDiscord $ def & token   .~ botConfig^.botToken
                     & onEvent .~ withEnv . onDiscordEvent
                     & onStart .~ withEnv onBotStartedStartNotifiers
                     & gatewayIntent .~ (def & messageContent .~ False)

  echo $ "A fatal error occurred: " <> botTerminationError
