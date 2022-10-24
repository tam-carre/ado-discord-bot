{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.SecretBaseRole (secretBaseRoleCmd) where

-- Ado Bot modules
import DiscordBot.SlashCommand    (roleCmd, SlashCommand, IsOptionRequired (..))
import DiscordBot.Guilds.Settings (GuildSettings (..))

-------------------------------------------------------------------------------

secretBaseRoleCmd :: SlashCommand
secretBaseRoleCmd = roleCmd "secretbaserole"
  "will receive notifications for Ado's Secret Base streams"
  Optional
  $ \newVal settings -> settings { secretBaseRole = newVal }
