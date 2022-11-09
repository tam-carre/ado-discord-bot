{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.SecretBase (secretBaseCmd) where

-- Ado Bot modules
import Lenses
import DiscordBot.SlashCommand (notificationChCmd, SlashCommand)

-------------------------------------------------------------------------------

secretBaseCmd :: SlashCommand
secretBaseCmd = notificationChCmd "secretbase"
  "Ado's Secret Base stream"
  (set secretBaseCh)
