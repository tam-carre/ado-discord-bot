{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.CommunityRole (communityRoleCmd) where

-- Ado Bot modules
import Lenses
import DiscordBot.SlashCommand (roleCmd, SlashCommand, IsOptionRequired (..))

-------------------------------------------------------------------------------

communityRoleCmd :: SlashCommand
communityRoleCmd = roleCmd "communityrole"
  "will receive notifications for Ado's YouTube community posts"
  Optional
  (set communityPostRole)
