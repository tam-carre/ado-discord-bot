{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

-- Ado Bot modules
import Config.BotConfig             (botConfig)
import Config.Type                  (BotConfig (..))
import DiscordBot.Events            (onDiscordEvent)
import DiscordBot.Events.BotStarted (onBotStarted)

-- Downloaded libraries
import Discord       (runDiscord, def, RunDiscordOpts (..))
import Discord.Types (GatewayIntent (..))

------------------------------------------------------------------------------

main :: IO ()
main = do
  putTextLn "Application started."
  botTerminationError <- runDiscord settings
  putTextLn $ "A fatal error occurred: " <> botTerminationError

settings :: RunDiscordOpts
settings = def
  { discordToken         = botConfig.botToken
  , discordOnEvent       = onDiscordEvent
  , discordOnStart       = onBotStarted
  , discordGatewayIntent = def { gatewayIntentMessageContent = False }
  }
