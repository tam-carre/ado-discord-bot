{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
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
import Discord       (DiscordHandler)
import Discord.Types (GuildMember, GuildId, InteractionId, InteractionToken)
import Data.Acid     (AcidState)
import Discord.Interactions
  ( Interaction (..)
  , ApplicationCommandData (..)
  , MemberOrUser (..)
  , OptionsData
  )

-------------------------------------------------------------------------------

onInteractionCreate :: AcidState SettingsDb -> Interaction -> DiscordHandler ()
onInteractionCreate db i = case interactionToSlashCommand i of
  Left err ->
    echo err

  Right SlashCommandData { slashCmd, mem, guildId, opts, intr } -> do
    gSettings <- getSettings db guildId
    if getPermLvl gSettings mem < slashCmd.permLvl
      then  replyEmbed intr "Insufficient permissions."
      else  slashCmd.handler db intr mem guildId opts

data SlashCommandData = SlashCommandData
  { slashCmd :: SlashCommand
  , mem      :: GuildMember
  , guildId  :: GuildId
  , opts     :: Maybe OptionsData
  , intr     :: (InteractionId, InteractionToken)
  }

interactionToSlashCommand :: Interaction -> Either Text SlashCommandData
interactionToSlashCommand = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = slash@ApplicationCommandDataChatInput {} } ->
      case cmdByName slash.applicationCommandDataName of
        Just found ->
          case (cmd.interactionUser, cmd.interactionGuildId) of
            (MemberOrUser (Left mem), Just guildId) ->
              Right $ SlashCommandData
                { slashCmd = found
                , mem      = mem
                , guildId  = guildId
                , opts     = slash.optionsData
                , intr     = (cmd.interactionId, cmd.interactionToken)
                }

            (_, Nothing) -> Left "Got slash cmd w/o channel (channel deleted?)"

            _ -> Left "Got slash cmd w/ no guild member (they likely left)"

        Nothing -> Left "Somehow got unknown slash command (registrations out of date?)"

  _ -> Left "Unexpected/unsupported interaction type"
