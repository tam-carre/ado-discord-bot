{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.SecretBase (secretBaseCmd) where

-- Ado Bot modules
import DiscordBot.SlashCommand    (notificationChCmd, SlashCommand)
import DiscordBot.Guilds.Settings (GuildSettings (..))

-------------------------------------------------------------------------------

secretBaseCmd :: SlashCommand
secretBaseCmd = notificationChCmd "secretbase"
  "Ado's Secret Base stream"
  (\newVal opts -> opts { secretBaseCh = newVal })
