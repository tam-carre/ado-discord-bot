{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.Ping (pingCmd) where

-- Ado Bot modules
import DiscordBot.Perms       (PermLvl (..))
import DiscordBot.SendMessage (replyEmbed)

import DiscordBot.SlashCommand
  ( slash
  , SlashProps (..)
  , optionString
  , getStrOpt
  , required
  , SlashCommand
  )

-------------------------------------------------------------------------------

pingCmd :: SlashCommand
pingCmd = slash $ SlashProps
  { name    = "ping"
  , desc    = "Sends an input back"
  , permLvl = PermLvlUser
  , options = [ optionString "input" "The text to send back" ]
  , handler = \_db intr _mem _guildId opts -> do
      input <- required getStrOpt "input" opts

      replyEmbed intr input
  }
