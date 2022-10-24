{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DiscordBot.Events.DiscordAPI.InteractionCreate
  ( onInteractionCreate
  ) where

-- Ado Bot modules
import DiscordBot.Guilds.Settings (SettingsDb, getSettings)
import DiscordBot.SendMessage     (replyEmbed)
import DiscordBot.Commands        (cmdByName)
import DiscordBot.SlashCommand    (SlashCommand (..))
import DiscordBot.Perms           (getPermLvl)

-- Downloaded libraries
import Discord   (DiscordHandler)
import Data.Acid (AcidState)

import Discord.Interactions
  ( Interaction (..)
  , ApplicationCommandData (..)
  , MemberOrUser (..)
  )

-------------------------------------------------------------------------------

onInteractionCreate :: AcidState SettingsDb -> Interaction -> DiscordHandler ()
onInteractionCreate db = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = slash@ApplicationCommandDataChatInput {} } ->
      case cmdByName slash.applicationCommandDataName of
        Just found ->
          case (cmd.interactionUser, cmd.interactionGuildId) of
            (MemberOrUser (Left mem), Just guildId) -> do
              gSettings <- getSettings db guildId
              if getPermLvl gSettings mem < found.permLvl
                then  replyEmbed intr "Insufficient permissions."
                else  found.handler db intr mem guildId slash.optionsData
                where intr = (cmd.interactionId, cmd.interactionToken)

            (_, Nothing) -> echo "Got slash cmd w/o channel (channel deleted?)"

            _ -> echo "Got slash cmd w/ no guild member (they likely left)"

        Nothing -> echo "Somehow got unknown slash command (registrations out of date?)"

  _ -> pass -- Unexpected/unsupported interaction type
