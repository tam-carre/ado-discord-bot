{-# OPTIONS -Wno-missing-fields #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
  , roleCmd
  , SlashCommand (..)
  , SlashProps (..)
  , IsOptionRequired (..)
  ) where

-- Ado Bot modules
import App                           (App)
import App.Types                     (Db (..))
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
required getter name' opts = getter name' opts <&> \case
  Just a -> a
  Nothing -> error "Asked for required option but it wasn't registered as such."

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
  -> (Maybe Word64 -> GuildSettings -> GuildSettings)
  -> SlashCommand
notificationChCmd name' thingToNotify setter = slash $ SlashProps
  { _name    = name'
  , _desc    = "Sets or clears the channel in which to send " <> thingToNotify <> " notifications"
  , _permLvl = PermLvlBotManager
  , _options =
      [ optionCh "channel" $
          "The channel (if any) in which to send " <> thingToNotify <> " notifications"
      ]
  , _handler = \intr _mem gid' opts -> do
      chanId <- getChOpt "channel" opts

      db <- asks _settingsDb
      changeSettings db gid' $ setter (w64 <$> chanId)

      replyEmbed intr $ case chanId of
        Nothing -> "Successfully disabled notifications for " <> thingToNotify <> "s."
        Just id' -> "Successfully set <#" <> show id' <> "> as the notification channel for " <> thingToNotify <> "s."
  }

roleCmd :: Text -> Text -> IsOptionRequired -> (Maybe Word64 -> GuildSettings -> GuildSettings) -> SlashCommand
roleCmd name' purpose isRequired setter = slash $ SlashProps
  { _name    = name'
  , _desc    = "Sets the role which " <> purpose
  , _permLvl = PermLvlBotManager
  , _options =
      [ maybeRequired isRequired optionRole "role" $ "The role which " <> purpose
      ]
  , _handler = \intr _mem gid' opts -> do
      roleId <- getRoleOpt "role" opts

      db <- asks _settingsDb
      changeSettings db gid' $ setter (w64 <$> roleId)

      replyEmbed intr $ case roleId of
        Nothing -> "Successfully unset the role which " <> purpose <> "."
        Just id' -> "Successfully set <@&" <> show id' <> "> as the role which "<> purpose <>"."
  }

data IsOptionRequired
  = Required
  | Optional

maybeRequired :: IsOptionRequired -> (Text -> Text -> OptionValue) -> Text -> Text -> OptionValue
maybeRequired Required = requiredOpt
maybeRequired Optional = identity
