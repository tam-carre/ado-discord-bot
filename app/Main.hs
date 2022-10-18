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

import qualified Data.Text.IO as TIO

------------------------------------------------------------------------------

main :: IO ()
main = do
  TIO.putStrLn "Application started."

  botTerminationError <- runDiscord $ def
    { discordToken         = botConfig.botToken
    , discordOnEvent       = onDiscordEvent
    , discordOnStart       = onBotStarted
    , discordGatewayIntent = def { gatewayIntentMessageContent = False }
    }

  TIO.putStrLn $ "A fatal error occurred:" <> botTerminationError
