{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.ModRole (modRoleCmd) where

-- Ado Bot modules
import DiscordBot.SlashCommand    (roleCmd, SlashCommand, IsOptionRequired (..))
import DiscordBot.Guilds.Settings (GuildSettings (..))

-------------------------------------------------------------------------------

modRoleCmd :: SlashCommand
modRoleCmd = roleCmd "modrole" "can manage this bot" Required $
  \newVal settings -> settings { modRole = newVal }
