{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.DiscordAPI.Ready (onReady) where

-- Ado Bot modules
import Lenses
import DiscordBot.Commands     (appCommands)
import DiscordBot.SlashCommand (SlashCommand (..))

-- Downloaded libraries
import Discord              (DiscordHandler, RestCallErrorCode (..), restCall)
import Discord.Types        (ApplicationId)
import Discord.Requests     (ApplicationCommandRequest (..))
import Discord.Interactions (ApplicationCommand (..))

-------------------------------------------------------------------------------

onReady :: ApplicationId -> DiscordHandler ()
onReady appId = do
  echo "Bot ready!"

  appCmdRegistrations <- mapM (tryRegistering appId) appCommands

  case sequence appCmdRegistrations of
    Left _err -> echo "[!] Failed to register some commands (must be lowercase)"
    Right cmds -> do
      echo $ "Registered " <> show (length cmds) <> " command(s)."
      unregisterOutdatedCmds appId cmds

tryRegistering ::
  ApplicationId
  -> SlashCommand
  -> DiscordHandler (Either RestCallErrorCode ApplicationCommand)
tryRegistering appId cmd = case cmd^.registration of
  Just reg -> restCall    $ CreateGlobalApplicationCommand appId reg
  Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

unregisterOutdatedCmds :: ApplicationId -> [ApplicationCommand] -> DiscordHandler ()
unregisterOutdatedCmds appId validCmds = do
  registered <- restCall $ GetGlobalApplicationCommands appId
  case registered of
    Left err -> echo $ "Failed to get registered slash commands: " <> show err
    Right cmds -> do
      let validIds    = map applicationCommandId validCmds
          outdatedIds = filter (`notElem` validIds) (cmds <&> view id)
      mapM_ (restCall . DeleteGlobalApplicationCommand appId) outdatedIds
