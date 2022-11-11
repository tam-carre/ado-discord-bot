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

cmdByName :: Text -> Maybe SlashCommand
cmdByName cmdName = find ((==) cmdName  . view name) appCommands

appCommands :: [SlashCommand]
appCommands =
  [ communityCmd
  , secretBaseCmd
  , relayCmd
  , modRoleCmd
  ] where
  communityCmd =
    notificationChCmd "community"
      "Ado's community post"
      communityPostCh
      "will receive notifications for Ado's YouTube community posts"
      communityPostRole

  secretBaseCmd =
    notificationChCmd "secretbase"
      "Ado's Secret Base stream"
      secretBaseCh
      "will receive notifications for Ado's Secret Base streams/vids"
      secretBaseRole

  relayCmd =
    notificationChCmdNoRole "relay"
      "YouTube livechat TL and Ado's own message"
      relayCh

  modRoleCmd =
    roleCmd "modrole" "can manage this bot" modRole
