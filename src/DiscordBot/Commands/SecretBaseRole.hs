{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.SecretBaseRole (secretBaseRoleCmd) where

-- Ado Bot modules
import Lenses
import DiscordBot.SlashCommand (roleCmd, SlashCommand, IsOptionRequired (..))

-------------------------------------------------------------------------------

secretBaseRoleCmd :: SlashCommand
secretBaseRoleCmd = roleCmd "secretbaserole"
  "will receive notifications for Ado's Secret Base streams"
  Optional
  (set secretBaseRole)
