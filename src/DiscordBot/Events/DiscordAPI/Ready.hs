{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.DiscordAPI.Ready (onReady) where

-- Ado Bot modules
import DiscordBot.Commands     (appCommands)
import DiscordBot.SlashCommand (SlashCommand (..))

-- Downloaded libraries
import Discord          (DiscordHandler, RestCallErrorCode (..), restCall)
import Discord.Types    (ApplicationId)
import Discord.Requests (ApplicationCommandRequest (..))

-------------------------------------------------------------------------------

onReady :: ApplicationId -> DiscordHandler ()
onReady appId = do
  echo "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering appCommands

  echo $ if all isRight appCmdRegistrations
    then "Registered " <> show (length appCmdRegistrations) <> " command(s)."
    else "[!] Failed to register some commands."

  where
  tryRegistering cmd = case registration cmd of
    Just reg -> restCall    $ CreateGlobalApplicationCommand appId reg
    Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""
