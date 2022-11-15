{-# LANGUAGE TypeApplications #-}

module App.Discord.Events.Gateway.InteractionCreate (onInteractionCreate) where

import App                         (App)
import App.Discord.Commands        (cmdByName)
import App.Discord.Guilds.Settings (getSettings)
import App.Discord.Perms           (getPermLvl)
import App.Discord.SendMessage     (replyEmbed)
import App.Discord.SlashCommand    (SlashCommand (..))
import App.Lenses                  (_Just, _Left, applicationCommandData, guildId, handler, name,
                                    optionsData, permLvl, to, user, (^.), (^?))
import App.Utils                   (onFail)
import Discord.Interactions        (Interaction, MemberOrUser (..), OptionsData)
import Discord.Types               (GuildId, GuildMember, User)
import Relude.Extra.Newtype        (un)

----------------------------------------------------------------------------------------------------

onInteractionCreate ∷ Interaction → App ()
onInteractionCreate i = case intrData i of
  Left err → echo err
  Right (slashCmd, member, gid, opts) → do
    gSettings ← getSettings gid
    if getPermLvl gSettings member < slashCmd^.permLvl
      then replyEmbed i "Insufficient permissions."
      else (slashCmd^.handler) i member gid opts

intrData ∷ Interaction → Either Text (SlashCommand, GuildMember, GuildId, Maybe OptionsData)
intrData intr = do
  slash  ← intr^?applicationCommandData
             & onFail "Unexpected/unsupported interaction type"
  found  ← cmdByName (slash^.name)
             & onFail "Unknown slash command (regs out of date?)"
  member ← intr^?user.to (un @(Either GuildMember User))._Left
             & onFail "Slash cmd has no guild member (they likely left)"
  gid    ← intr^?guildId._Just
             & onFail "Slash cmd has no channel (channel deleted?)"

  pure (found, member, gid, slash^?optionsData._Just)
