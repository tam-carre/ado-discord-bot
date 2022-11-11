{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DiscordBot.SlashCommand
  ( slash
  , optionString
  , optionCh
  , optionRole
  , getChOpt
  , getStrOpt
  , getRoleOpt
  , required
  , requiredOpt
  , notificationChCmd
  , notificationChCmdNoRole
  , roleCmd
  , SlashCommand (..)
  , SlashProps (..)
  ) where

-- Ado Bot modules
import App                           (App)
import DiscordBot.SlashCommand.Types (SlashCommand (..), SlashProps (..))
import DiscordBot.Perms              (PermLvl (..))
import DiscordBot.SendMessage        (replyEmbed)
import DiscordBot.Guilds.Settings    (GuildSettings, changeSettings, w64)
import Lenses hiding (required)
import qualified Lenses as L

-- Downloaded libraries
import Discord.Types (ChannelId, RoleId)
import Discord.Interactions
  ( createChatInput
  , CreateApplicationCommand (..)
  , Options (OptionsValues)
  , OptionValue (..)
  , OptionsData (OptionsDataValues)
  , ApplicationCommandChannelType (ApplicationCommandChannelTypeGuildText)
  , OptionDataValue (..)
  )

-------------------------------------------------------------------------------

slash :: SlashProps -> SlashCommand
slash (SlashProps _name _desc _permLvl _options _handler) = SlashCommand
  { _name
  , _desc
  , _permLvl
  , _options
  , _handler
  , _registration =
    createChatInput _name _desc >>= \case
      slashCmd@CreateApplicationCommandChatInput {} ->
          Just $ slashCmd & options ?~ OptionsValues _options

      _ -> Nothing
  }

optionString :: Text -> Text -> OptionValue
optionString name' description' =
  OptionValueString {}
    & name          .~ name'
    & localizedName .~ Nothing
    & description   .~ description'
    & L.required    .~ True
    & stringChoices .~ Left False
    & stringMinLen  .~ Nothing
    & stringMaxLen  .~ Nothing
    & localizedDescription .~ Nothing

optionCh :: Text -> Text -> OptionValue
optionCh name' description' =
  OptionValueChannel {}
    & name          .~ name'
    & localizedName .~ Nothing
    & description   .~ description'
    & L.required    .~ False
    & channelTypes  ?~ [ApplicationCommandChannelTypeGuildText]
    & localizedDescription .~ Nothing

optionRole :: Text -> Text -> OptionValue
optionRole name' description' =
  OptionValueRole {}
    & name          .~ name'
    & localizedName .~ Nothing
    & description   .~ description'
    & L.required    .~ False
    & localizedDescription .~ Nothing

requiredOpt :: (Text -> Text -> OptionValue) -> Text -> Text -> OptionValue
requiredOpt builder name' description' = builder name' description' & L.required .~ True

required :: (Text -> Maybe OptionsData -> App (Maybe a)) -> Text -> Maybe OptionsData -> App a
required getter name' opts = getter name' opts
  <&> fromMaybe (error "Asked for required option but it wasn't registered as such.")

getStrOpt :: Text -> Maybe OptionsData -> App (Maybe Text)
getStrOpt = getOpt toStr where
  toStr (OptionDataValueString _ (Right str)) = Just str
  toStr _                                     = Nothing

getChOpt :: Text -> Maybe OptionsData -> App (Maybe ChannelId)
getChOpt = getOpt toCh where
  toCh (OptionDataValueChannel _ cId) = Just cId
  toCh _                              = Nothing

getRoleOpt :: Text -> Maybe OptionsData -> App (Maybe RoleId)
getRoleOpt = getOpt toRole where
  toRole (OptionDataValueRole _ rId) = Just rId
  toRole _                           = Nothing

getOpt :: (OptionDataValue -> Maybe a) -> Text -> Maybe OptionsData -> App (Maybe a)
getOpt toA name' (opt name' toA -> a) = pure a

opt :: Text -> (OptionDataValue -> Maybe a) -> Maybe OptionsData -> Maybe a
opt n toA = \case
  Just (OptionsDataValues (findOpt n -> Just (toA -> a))) -> a
  _ -> Nothing
  where findOpt optName = find ((==) optName . optionDataValueName)

notificationChCmd ::
  Text
  -> Text
  -> Lens' GuildSettings (Maybe Word64)
  -> Text
  -> Lens' GuildSettings (Maybe Word64)
  -> SlashCommand
notificationChCmd name' thingToNotify chanL rolePurpose roleL =
  notificationChCmd' name' thingToNotify chanL (Just (rolePurpose, roleL))

notificationChCmdNoRole :: Text -> Text -> Lens' GuildSettings (Maybe Word64) -> SlashCommand
notificationChCmdNoRole name' thingToNotify chanL =
  notificationChCmd' name' thingToNotify chanL Nothing

notificationChCmd' ::
  Text
  -> Text
  -> Lens' GuildSettings (Maybe Word64)
  -> Maybe (Text, Lens' GuildSettings (Maybe Word64))
  -> SlashCommand
notificationChCmd' name' thingToNotify chanL rolePurposeAndL =
  slash $ SlashProps
    { _name    = name'
    , _desc    = "Sets/clears the chan"
               <> maybe "" (const "+role") rolePurposeAndL
               <> " in which to send " <> thingToNotify <> " notifs"
    , _permLvl = PermLvlBotManager
    , _options =
      optionCh "channel"
          ("The channel (if any) in which to send " <> thingToNotify <> " notifications")
      : ( case rolePurposeAndL of
            Nothing -> []
            Just (purpose, _) -> [optionRole "role" $ "The role which " <> purpose]
        )
    , _handler = \intr _mem gid' opts -> do
        chanId <- getChOpt "channel" opts
        roleId <- getRoleOpt "role" opts

        changeSettings gid' $ set chanL (w64 <$> chanId)

        case rolePurposeAndL of
          Nothing -> pass
          Just (_, roleL) -> changeSettings gid' $ set roleL (w64 <$> roleId)

        replyEmbed intr $ case chanId of
          Nothing -> "Disabled notifications for " <> thingToNotify <> "s."
          Just id' ->
            "Set <#" <> show id' <> "> as the notification channel for " <> thingToNotify <> "s."
            <> case (fst <$> rolePurposeAndL, roleId) of
                 (Just purpose, Nothing) ->
                   "\nUnset the role which " <> purpose <> "."
                 (Just purpose, Just rid) ->
                  "\nSet <@&" <> show rid <> "> as the role which "<> purpose <> "."
                 (Nothing, _) ->
                    ""
    }

roleCmd :: Text -> Text -> Lens' GuildSettings (Maybe Word64) -> SlashCommand
roleCmd name' purpose roleL = slash $ SlashProps
  { _name    = name'
  , _desc    = "Sets the role which " <> purpose
  , _permLvl = PermLvlBotManager
  , _options =
      [ requiredOpt optionRole "role" $ "The role which " <> purpose
      ]
  , _handler = \intr _mem gid' opts -> do
      roleId <- getRoleOpt "role" opts

      changeSettings gid' $ set roleL (w64 <$> roleId)

      replyEmbed intr $ case roleId of
        Nothing -> "Unset the role which " <> purpose <> "."
        Just id' -> "Set <@&" <> show id' <> "> as the role which "<> purpose <>"."
  }
