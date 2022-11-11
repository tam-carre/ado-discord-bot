{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands (appCommands, cmdByName) where

-- Ado Bot modules
import Lenses
import DiscordBot.SlashCommand
  ( SlashCommand (..)
  , notificationChCmd
  , notificationChCmdNoRole
  , roleCmd
  )

-------------------------------------------------------------------------------

appCommands :: [SlashCommand]
appCommands =
  [ communityCmd
  , secretBaseCmd
  , relayCmd
  , modRoleCmd
  ]

cmdByName :: Text -> Maybe SlashCommand
cmdByName cmdName = find ((==) cmdName  . view name) appCommands

communityCmd :: SlashCommand
communityCmd = notificationChCmd "community"
  "Ado's community post"
  communityPostCh
  "will receive notifications for Ado's YouTube community posts"
  communityPostRole

secretBaseCmd :: SlashCommand
secretBaseCmd = notificationChCmd "secretbase"
  "Ado's Secret Base stream"
  secretBaseCh
  "will receive notifications for Ado's Secret Base streams/vids"
  secretBaseRole

relayCmd :: SlashCommand
relayCmd = notificationChCmdNoRole "relay"
  "YouTube livechat TL and Ado's own message"
  relayCh

modRoleCmd :: SlashCommand
modRoleCmd = roleCmd "modrole" "can manage this bot" modRole
