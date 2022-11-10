{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE LambdaCase        #-}

module DiscordBot.Events.DiscordAPI.InteractionCreate
  ( onInteractionCreate
  ) where

-- Ado Bot modules
import Lenses
import App                        (App)
import DiscordBot.Guilds.Settings (getSettings)
import DiscordBot.SendMessage     (replyEmbed)
import DiscordBot.Commands        (cmdByName)
import DiscordBot.SlashCommand    (SlashCommand (..))
import DiscordBot.Perms           (getPermLvl)
import qualified Lenses as L

-- Downloaded libraries
import Discord.Types (GuildMember, GuildId, InteractionId, InteractionToken)
import Discord.Interactions
  ( Interaction (..)
  , ApplicationCommandData (..)
  , MemberOrUser (..)
  , OptionsData
  )

-------------------------------------------------------------------------------

onInteractionCreate :: Interaction -> App ()
onInteractionCreate i = case interactionToSlashCommand i of
  Left err -> echo err
  Right SlashCommandData { slashCmd, mem, gid, opts, intr } -> do
    gSettings <- getSettings gid
    if getPermLvl gSettings mem < slashCmd^.permLvl
      then replyEmbed intr "Insufficient permissions."
      else (slashCmd^.handler) intr mem gid opts

data SlashCommandData = SlashCommandData
  { slashCmd :: SlashCommand
  , mem      :: GuildMember
  , gid      :: GuildId
  , opts     :: Maybe OptionsData
  , intr     :: (InteractionId, InteractionToken)
  }

interactionToSlashCommand :: Interaction -> Either Text SlashCommandData
interactionToSlashCommand = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = slash@ApplicationCommandDataChatInput {} } ->
      case cmdByName $ slash^.name of
        Just found ->
          case (cmd^?user, cmd^?guildId._Just) of
            (Just (MemberOrUser (Left mem')), Just gid') ->
              Right $ SlashCommandData
                { slashCmd = found
                , mem      = mem'
                , gid      = gid'
                , opts     = slash ^? L.optionsData._Just
                , intr     = (cmd^.id, cmd^.token)
                }

            (_, Nothing) -> Left "Got slash cmd w/o channel (channel deleted?)"

            _ -> Left "Got slash cmd w/ no guild member (they likely left)"

        Nothing -> Left "Somehow got unknown slash command (registrations out of date?)"

  _ -> Left "Unexpected/unsupported interaction type"
