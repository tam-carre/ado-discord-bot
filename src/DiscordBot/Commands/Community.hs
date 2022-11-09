{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.Community (communityCmd) where

-- Ado Bot modules
import Lenses
import DiscordBot.SlashCommand (notificationChCmd, SlashCommand)

-------------------------------------------------------------------------------

communityCmd :: SlashCommand
communityCmd = notificationChCmd "community"
  "Ado's community post"
  (set communityPostCh)
