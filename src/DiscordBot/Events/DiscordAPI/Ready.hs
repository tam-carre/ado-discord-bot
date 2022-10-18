{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.DiscordAPI.Ready (onReady) where

-- Ado Bot modules
import DiscordBot.Commands     (appCommands)
import DiscordBot.SlashCommand (SlashCommand (..))
import DiscordBot.Utils        (putLn)

-- Downloaded libraries
import Discord.Types (ApplicationId)

import qualified Data.Text        as T
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
  putLn "Bot ready!"

  appCmdRegistrations <- mapM attemptRegistering appCommands

  putLn $ if all isRight appCmdRegistrations
    then "Registered " <> len appCmdRegistrations <> " command(s)."
    else "[!] Failed to register some commands."

  where
  len = T.pack . show . length
  attemptRegistering cmd = case registration cmd of
    Just reg -> restCall $ R.CreateGlobalApplicationCommand appId reg
    Nothing  -> return . Left $ RestCallErrorCode 0 "" ""
