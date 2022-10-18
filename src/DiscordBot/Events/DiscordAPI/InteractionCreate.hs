{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DiscordBot.Events.DiscordAPI.InteractionCreate
  ( onInteractionCreate
  ) where

-- Ado Bot modules
import DiscordBot.SendMessage  (reply)
import DiscordBot.Commands     (cmdByName)
import DiscordBot.SlashCommand (SlashCommand (..))
import DiscordBot.Utils        (putLn)
import DiscordBot.Perms        (getPermLvl)

-- Downloaded libraries
import Discord (DiscordHandler)

import Discord.Interactions
  ( Interaction (..)
  , ApplicationCommandData (..)
  , MemberOrUser (..)
  )

-------------------------------------------------------------------------------

onInteractionCreate :: Interaction -> DiscordHandler ()
onInteractionCreate = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = slash@ApplicationCommandDataChatInput {} } ->
      case cmdByName slash.applicationCommandDataName of
        Just found ->
          case cmd.interactionUser of
            MemberOrUser (Left mem) ->
              if getPermLvl mem < found.permLvl
                then  reply intr "Insufficient permissions."
                else  found.handler intr mem slash.optionsData
                where intr = (cmd.interactionId, cmd.interactionToken)

            _ -> putLn "Got slash cmd w/ no guild member (member likely left)"

        Nothing -> putLn "Somehow got unknown slash command"

  _ -> pure ()
