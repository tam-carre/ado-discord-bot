{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.ModRole (modRoleCmd) where

-- Ado Bot modules
import Lenses
import DiscordBot.SlashCommand (roleCmd, SlashCommand, IsOptionRequired (..))

-------------------------------------------------------------------------------

modRoleCmd :: SlashCommand
modRoleCmd = roleCmd "modrole" "can manage this bot" Required $ set modRole
