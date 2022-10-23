{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.Community (communityCmd) where

-- Ado Bot modules
import DiscordBot.Perms           (PermLvl (..))
import DiscordBot.SendMessage     (replyEmbed)
import DiscordBot.Guilds.Settings (changeSettings, w64 , GuildSettings (..))

import DiscordBot.SlashCommand
  ( slash
  , SlashProps (..)
  , optionCh
  , SlashCommand
  , getChOpt
  )

-------------------------------------------------------------------------------

communityCmd :: SlashCommand
communityCmd = slash $ SlashProps
  { name    = "community"
  , desc    = "Sets or clears the channel in which to send Ado's community post notifications"
  , permLvl = PermLvlBotManager
  , options =
      [ optionCh "channel" "The channel (if any) in which to send Ado's community post notifications"
      ]
  , handler = \db intr _mem guildId opts -> do
      chanId <- getChOpt "channel" opts
      
      changeSettings db guildId $ \s -> s { communityPostCh = w64 <$> chanId }

      replyEmbed intr $ case chanId of
        Nothing -> "Successfully disabled notifications for Ado's community posts."
        Just id -> "Successfully set <#" <> show id <> "> as the notification channel for Ado's community posts."
  }

