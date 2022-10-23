{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.CommunityRole (communityRoleCmd) where

-- Ado Bot modules
import DiscordBot.SlashCommand    (roleCmd, SlashCommand, IsOptionRequired (..))
import DiscordBot.Guilds.Settings (GuildSettings (..))

-------------------------------------------------------------------------------

communityRoleCmd :: SlashCommand
communityRoleCmd = roleCmd "communityrole"
  "will receive notifications for Ado's YouTube community posts"
  Optional
  $ \newVal settings -> settings { communityPostRole = newVal }
