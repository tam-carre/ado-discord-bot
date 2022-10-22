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

            _ -> putTextLn "Got slash cmd w/ no guild member (they likely left)"

        Nothing -> putTextLn "Somehow got unknown slash command"

  _ -> pure ()
