{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, FlexibleInstances, ImpredicativeTypes #-}

module App.Discord.SlashCommand
  ( SlashCommand (..)
  , notificationChCmd
  , notificationChCmdNoRole
  , optionCh
  , optionRole
  , optionString
  , roleCmd
  ) where

import App                            (App)
import App.Discord.Guilds.Settings    (GuildSettings, changeSettings, w64)
import App.Discord.Perms              (PermLvl (..))
import App.Discord.SendMessage        (replyEmbed)
import App.Discord.SlashCommand.Types (SlashCommand (..))
import App.Lenses                     (Lens', Prism', _2, _Just, _OptionDataValueChannel,
                                       _OptionDataValueRole, _OptionsDataValues, channelTypes, desc,
                                       description, handler, localizedDescription, localizedName,
                                       name, options, permLvl, reg, required, set, stringChoices,
                                       stringMaxLen, stringMinLen, to, (.~), (?~), (^.), (≫^?))
import Discord.Interactions           (ApplicationCommandChannelType (..), CreateApplicationCommand,
                                       OptionDataValue (..), OptionValue (..),
                                       Options (OptionsValues), OptionsData, createChatInput)
import Discord.Types                  (ChannelId, RoleId)

----------------------------------------------------------------------------------------------------

chatInput ∷ Text → Text → [OptionValue] → Maybe CreateApplicationCommand
chatInput name' desc' options' =
  createChatInput name' desc' & _Just.options ?~ OptionsValues options'

optDef ∷ Text → Text → OptionValue → OptionValue
optDef name' desc'
  = name          .~ name'
  ⋙ description   .~ desc'
  ⋙ localizedName .~ Nothing
  ⋙ required      .~ False
  ⋙ localizedDescription .~ Nothing

req ∷ OptionValue → OptionValue
req = set required True

optionString ∷ Text → Text → OptionValue
optionString name' description' =
  OptionValueString {} & optDef name' description'
    & stringChoices .~ Left False
    & stringMinLen  .~ Nothing
    & stringMaxLen  .~ Nothing
    & req

optionCh ∷ Text → Text → OptionValue
optionCh name' description' =
  OptionValueChannel {} & optDef name' description'
    & channelTypes ?~ [ApplicationCommandChannelTypeGuildText]

optionRole ∷ Text → Text → OptionValue
optionRole name' description' = OptionValueRole {} & optDef name' description'

getOpt ∷ Prism' OptionDataValue (a, b) → Text → Maybe OptionsData → App (Maybe b)
getOpt kind name' optData =
  pure $ optData≫^?_OptionsDataValues.to (find ((name'≡) . (^.name)))._Just.kind._2

getChOpt ∷ Text → Maybe OptionsData → App (Maybe ChannelId)
getChOpt = getOpt _OptionDataValueChannel

getRoleOpt ∷ Text → Maybe OptionsData → App (Maybe RoleId)
getRoleOpt = getOpt _OptionDataValueRole

notificationChCmd
  ∷ Text
  → Text
  → Lens' GuildSettings (Maybe Word64)
  → Text
  → Lens' GuildSettings (Maybe Word64)
  → SlashCommand
notificationChCmd name' thingToNotify chanL rolePurpose roleL =
  notificationChCmd' name' thingToNotify chanL $ Just (rolePurpose, roleL)

notificationChCmdNoRole ∷ Text → Text → Lens' GuildSettings (Maybe Word64) → SlashCommand
notificationChCmdNoRole name' thingToNotify chanL =
  notificationChCmd' name' thingToNotify chanL Nothing

notificationChCmd'
  ∷ Text
  → Text
  → Lens' GuildSettings (Maybe Word64)
  → Maybe (Text, Lens' GuildSettings (Maybe Word64))
  → SlashCommand
notificationChCmd' name' thingToNotify chanL rolePurposeAndL =
  let desc' = "Sets/clears the chan"
            ⊕ maybe "" (const "+role") rolePurposeAndL
            ⊕ " in which to send " ⊕ thingToNotify ⊕ " notifs"
  in SlashCommand {}
    & name    .~ name'
    & desc    .~ desc'
    & permLvl .~ PermLvlBotManager
    & reg     .~ chatInput name' desc'
                   (catMaybes
                     [ Just $ optionCh "channel"
                       ("The channel (if any) in which to send " ⊕ thingToNotify ⊕ " notifications")
                     , optionRole "role" . ("The role which " ⊕). fst <$> rolePurposeAndL
                     ]
                   )
    & handler .~
      \intr _mem gid' opts → do
        chanId ← getChOpt "channel" opts
        roleId ← getRoleOpt "role" opts

        changeSettings gid' $ set chanL (w64 <$> chanId)

        case rolePurposeAndL of
          Nothing         → pass
          Just (_, roleL) → changeSettings gid' $ set roleL (w64 <$> roleId)

        replyEmbed intr $ case chanId of
          Nothing → "Disabled notifications for " ⊕ thingToNotify ⊕ "s."
          Just id' →
            "Set <#" ⊕ show id' ⊕ "> as the notification channel for "
            ⊕ thingToNotify ⊕ "s."
            ⊕ case (fst <$> rolePurposeAndL, roleId) of
                 (Just purpose, Nothing) →
                   "\nUnset the role which " ⊕ purpose ⊕ "."
                 (Just purpose, Just rid) →
                  "\nSet <@&" ⊕ show rid ⊕ "> as the role which "  ⊕ purpose ⊕ "."
                 (Nothing, _) →
                    ""

roleCmd ∷ Text → Text → Lens' GuildSettings (Maybe Word64) → SlashCommand
roleCmd name' purpose roleL =
  let desc' = "Sets the role which " ⊕ purpose
   in SlashCommand {}
    & name    .~ name'
    & desc    .~ desc'
    & permLvl .~ PermLvlBotManager
    & reg     .~ chatInput name' desc' [req . optionRole "role" $ "The role which " ⊕ purpose]
    & handler .~
      \intr _mem gid' opts → do
        roleId ← getRoleOpt "role" opts

        changeSettings gid' $ set roleL (w64 <$> roleId)

        replyEmbed intr $ case roleId of
          Nothing  → "Unset the role which " ⊕ purpose ⊕ "."
          Just id' → "Set <@&" ⊕ show id' ⊕ "> as the role which " ⊕ purpose ⊕ "."
