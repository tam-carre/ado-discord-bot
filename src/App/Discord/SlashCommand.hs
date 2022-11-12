{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE DuplicateRecordFields, ImpredicativeTypes, LambdaCase, ViewPatterns #-}

module App.Discord.SlashCommand
  ( SlashCommand (..)
  , SlashProps (..)
  , getChOpt
  , getRoleOpt
  , getStrOpt
  , notificationChCmd
  , notificationChCmdNoRole
  , optionCh
  , optionRole
  , optionString
  , required
  , requiredOpt
  , roleCmd
  , slash
  ) where

import App                            (App)
import App.Discord.Guilds.Settings    (GuildSettings, changeSettings, w64)
import App.Discord.Perms              (PermLvl (..))
import App.Discord.SendMessage        (replyEmbed)
import App.Discord.SlashCommand.Types (SlashCommand (..), SlashProps (..))
import App.Lenses                     (Lens', channelTypes, desc, description, handler,
                                       localizedDescription, localizedName, name, options, permLvl,
                                       registration, set, stringChoices, stringMaxLen, stringMinLen,
                                       (.~), (?~), (^.))
import App.Lenses                     qualified as L
import Discord.Interactions           (ApplicationCommandChannelType (ApplicationCommandChannelTypeGuildText),
                                       CreateApplicationCommand (..), OptionDataValue (..),
                                       OptionValue (..), Options (OptionsValues),
                                       OptionsData (OptionsDataValues), createChatInput)
import Discord.Types                  (ChannelId, RoleId)

----------------------------------------------------------------------------------------------------

slash ∷ SlashProps → SlashCommand
slash props = SlashCommand {}
  & name    .~ props^.name
  & desc    .~ props^.desc
  & permLvl .~ props^.permLvl
  & options .~ props^.options
  & handler .~ props^.handler
  & registration .~ ( createChatInput (props^.name) (props^.desc) ≫= \case
                        slashCmd@CreateApplicationCommandChatInput {} →
                          Just $ slashCmd & options ?~ OptionsValues (props^.options)
                        _ → Nothing
                    )

optDef ∷ Text → Text → OptionValue → OptionValue
optDef name' desc' = name          .~ name'
                   ⋙ description   .~ desc'
                   ⋙ localizedName .~ Nothing
                   ⋙ L.required    .~ False
                   ⋙ localizedDescription .~ Nothing

optionString ∷ Text → Text → OptionValue
optionString name' description' =
  OptionValueString {}
    & optDef name' description'
    & L.required    .~ True
    & stringChoices .~ Left False
    & stringMinLen  .~ Nothing
    & stringMaxLen  .~ Nothing

optionCh ∷ Text → Text → OptionValue
optionCh name' description' =
  OptionValueChannel {}
    & optDef name' description'
    & channelTypes ?~ [ApplicationCommandChannelTypeGuildText]

optionRole ∷ Text → Text → OptionValue
optionRole name' description' = OptionValueRole {} & optDef name' description'

requiredOpt ∷ (Text → Text → OptionValue) → Text → Text → OptionValue
requiredOpt builder name' description' = builder name' description' & L.required .~ True

required ∷ (Text → Maybe OptionsData → App (Maybe a)) → Text → Maybe OptionsData → App a
required getter name' opts = getter name' opts
  <&> fromMaybe (error "Asked for required option but it wasn't registered as such.")

getStrOpt ∷ Text → Maybe OptionsData → App (Maybe Text)
getStrOpt = getOpt toStr where
  toStr (OptionDataValueString _ (Right str)) = Just str
  toStr _                                     = Nothing

getChOpt ∷ Text → Maybe OptionsData → App (Maybe ChannelId)
getChOpt = getOpt toCh where
  toCh (OptionDataValueChannel _ cId) = Just cId
  toCh _                              = Nothing

getRoleOpt ∷ Text → Maybe OptionsData → App (Maybe RoleId)
getRoleOpt = getOpt toRole where
  toRole (OptionDataValueRole _ rId) = Just rId
  toRole _                           = Nothing

getOpt ∷ (OptionDataValue → Maybe a) → Text → Maybe OptionsData → App (Maybe a)
getOpt toA name' (opt name' toA → a) = pure a

opt ∷ Text → (OptionDataValue → Maybe a) → Maybe OptionsData → Maybe a
opt n toA = \case
  Just (OptionsDataValues (findOpt n → Just (toA → a))) → a
  _                                                     → Nothing
  where findOpt optName = find ((optName ≡) . (^.name))

notificationChCmd ∷
  Text
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

notificationChCmd' ∷
  Text
  → Text
  → Lens' GuildSettings (Maybe Word64)
  → Maybe (Text, Lens' GuildSettings (Maybe Word64))
  → SlashCommand
notificationChCmd' name' thingToNotify chanL rolePurposeAndL =
  slash $ SlashProps {}
    & name    .~ name'
    & desc    .~ "Sets/clears the chan"
              ⊕ maybe "" (const "+role") rolePurposeAndL
              ⊕ " in which to send " ⊕ thingToNotify ⊕ " notifs"
    & permLvl .~ PermLvlBotManager
    & options .~ optionCh "channel"
                  ("The channel (if any) in which to send " ⊕ thingToNotify ⊕ " notifications")
               : maybe [] (\(p,_) → [optionRole "role" $ "The role which " ⊕ p]) rolePurposeAndL
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
  slash $ SlashProps {}
    & name    .~ name'
    & desc    .~ "Sets the role which " ⊕ purpose
    & permLvl .~ PermLvlBotManager
    & options .~ [requiredOpt optionRole "role" $ "The role which " ⊕ purpose]
    & handler .~
      \intr _mem gid' opts → do
        roleId ← getRoleOpt "role" opts

        changeSettings gid' $ set roleL (w64 <$> roleId)

        replyEmbed intr $ case roleId of
          Nothing  → "Unset the role which " ⊕ purpose ⊕ "."
          Just id' → "Set <@&" ⊕ show id' ⊕ "> as the role which " ⊕ purpose ⊕ "."
