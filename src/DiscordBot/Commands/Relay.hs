{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.Relay (relayCmd) where

-- Ado Bot modules
import DiscordBot.Guilds.Settings (GuildSettings (..))
import DiscordBot.SlashCommand    (notificationChCmd, SlashCommand)

-------------------------------------------------------------------------------

relayCmd :: SlashCommand
relayCmd = notificationChCmd "relay"
  "YouTube livechat TL and Ado's own message"
  (\newVal opts -> opts { relayCh = newVal })
