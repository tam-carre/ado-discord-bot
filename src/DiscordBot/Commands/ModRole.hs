{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.ModRole (modRoleCmd) where

-- Ado Bot modules
import DiscordBot.Perms           (PermLvl (..))
import DiscordBot.SendMessage     (replyEmbed)
import DiscordBot.Guilds.Settings (changeSettings, dId, GuildSettings (..))

import DiscordBot.SlashCommand
  ( slash
  , SlashProps (..)
  , optionRole
  , SlashCommand
  , getRoleOpt
  , required
  , requiredOpt
  )

-------------------------------------------------------------------------------

modRoleCmd :: SlashCommand
modRoleCmd = slash $ SlashProps
  { name    = "modrole"
  , desc    = "Sets the role which can manage this bot"
  , permLvl = PermLvlBotManager
  , options = [ requiredOpt optionRole "role" "The role which can manage this bot" ]
  , handler = \db intr _mem guildId opts -> do
      roleId <- required getRoleOpt "role" opts
      
      changeSettings db guildId $ \s -> s { modRole = dId roleId }

      replyEmbed intr $
        "Successfully set <@" <> show roleId <> "> as the role which can manage this bot."
  }

