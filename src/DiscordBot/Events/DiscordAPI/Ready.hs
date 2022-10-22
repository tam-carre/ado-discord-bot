{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DiscordBot.Events.DiscordAPI.Ready (onReady) where

-- Ado Bot modules
import DiscordBot.Commands     (appCommands)
import DiscordBot.SlashCommand (SlashCommand (..))

-- Downloaded libraries
import Discord.Types (ApplicationId)

import qualified Discord.Requests as R

import Discord
  ( DiscordHandler
  , RestCallErrorCode (RestCallErrorCode)
  , restCall
  )

-- Base
import Data.Either (isRight)

-------------------------------------------------------------------------------

onReady :: ApplicationId -> DiscordHandler ()
onReady appId = do
  putTextLn "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering appCommands

  putTextLn $ if all isRight appCmdRegistrations
    then "Registered " <> (show $ length appCmdRegistrations) <> " command(s)."
    else "[!] Failed to register some commands."

  where
  tryRegistering cmd = case registration cmd of
    Just reg -> restCall $ R.CreateGlobalApplicationCommand appId reg
    Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""
