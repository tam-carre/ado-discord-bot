{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module App.Discord.Events.Gateway.InteractionCreate (onInteractionCreate) where

import App                         (App)
import App.Discord.Commands        (cmdByName)
import App.Discord.Guilds.Settings (getSettings)
import App.Discord.Perms           (getPermLvl)
import App.Discord.SendMessage     (replyEmbed)
import App.Discord.SlashCommand    (SlashCommand (..))
import App.Lenses                  (_Just, guildId, handler, id, name, optionsData, permLvl, token,
                                    user, (^.), (^?))
import Discord.Interactions        (ApplicationCommandData (ApplicationCommandDataChatInput),
                                    Interaction (..), MemberOrUser (..), OptionsData)
import Discord.Types               (GuildId, GuildMember, InteractionId, InteractionToken)

----------------------------------------------------------------------------------------------------

onInteractionCreate ∷ Interaction → App ()
onInteractionCreate i = case interactionToSlashCommand i of
  Left err → echo err
  Right (slashCmd, member, gid, opts, intr) → do
    gSettings ← getSettings gid
    if getPermLvl gSettings member < slashCmd^.permLvl
      then replyEmbed intr "Insufficient permissions."
      else (slashCmd^.handler) intr member gid opts

interactionToSlashCommand ∷ Interaction → Either Text (SlashCommand, GuildMember, GuildId, Maybe OptionsData, (InteractionId, InteractionToken))
interactionToSlashCommand = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = slash@ApplicationCommandDataChatInput {} } →
      case cmdByName $ slash^.name of
        Just found →
          case (cmd^?user, cmd^?guildId._Just) of
            (Just (MemberOrUser (Left member)), Just gid) →
              pure (found, member, gid, slash ^? optionsData._Just, (cmd^.id, cmd^.token))

            (_, Nothing) → fail "Got slash cmd w/o channel (channel deleted?)"
            _            → fail "Got slash cmd w/ no guild member (they likely left)"
        Nothing → fail "Somehow got unknown slash command (registrations out of date?)"
  _ → fail "Unexpected/unsupported interaction type"
