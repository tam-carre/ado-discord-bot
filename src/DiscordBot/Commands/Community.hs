{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.Community (communityCmd) where

-- Ado Bot modules
import DiscordBot.Guilds.Settings (GuildSettings (..))
import DiscordBot.SlashCommand    (notificationChCmd, SlashCommand)

-------------------------------------------------------------------------------

communityCmd :: SlashCommand
communityCmd = notificationChCmd "community"
  "Ado's community post"
  (\newVal opts -> opts { communityPostCh = newVal })
