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
  { _name    = "ping"
  , _desc    = "Sends an input back"
  , _permLvl = PermLvlUser
  , _options = [ optionString "input" "The text to send back" ]
  , _handler = \_db intr _mem _guildId opts -> do
      input <- required getStrOpt "input" opts

      replyEmbed intr input
  }
