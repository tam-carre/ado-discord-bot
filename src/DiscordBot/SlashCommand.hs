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
import DiscordBot.Perms           (PermLvl (..))
import DiscordBot.SendMessage     (replyEmbed)
import DiscordBot.Guilds.Settings (SettingsDb, GuildSettings, changeSettings, w64)

-- Downloaded libraries
import Discord   (DiscordHandler)
import Data.Acid (AcidState)
import Discord.Types
  ( GuildMember
  , InteractionId
  , ChannelId
  , InteractionToken
  , GuildId
  , RoleId
  )
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

data SlashCommand = SlashCommand
  { name         :: Text
  , desc         :: Text
  , permLvl      :: PermLvl
  , options      :: [OptionValue]
  , handler      :: AcidState SettingsDb
                 -> (InteractionId, InteractionToken)
                 -> GuildMember
                 -> GuildId
                 -> Maybe OptionsData
                 -> DiscordHandler ()
  , registration :: Maybe CreateApplicationCommand
  }

data SlashProps = SlashProps
  { name    :: Text
  , desc    :: Text
  , permLvl :: PermLvl
  , options :: [OptionValue]
  , handler :: AcidState SettingsDb
            -> (InteractionId, InteractionToken)
            -> GuildMember
            -> GuildId
            -> Maybe OptionsData
            -> DiscordHandler ()
  }

slash :: SlashProps -> SlashCommand
slash (SlashProps name desc permLvl options handler) = SlashCommand
  { name
  , desc
  , permLvl
  , options
  , handler
  , registration =
    createChatInput name desc >>= \case
      slashCmd@CreateApplicationCommandChatInput {} ->
          Just $ slashCmd { createOptions = Just $ OptionsValues options }

      _ -> Nothing
  }

optionString :: Text -> Text -> OptionValue
optionString name description = OptionValueString
  { optionValueName                 = name
  , optionValueLocalizedName        = Nothing
  , optionValueDescription          = description
  , optionValueLocalizedDescription = Nothing
  , optionValueRequired             = True
  , optionValueStringChoices        = Left False
  , optionValueStringMinLen         = Nothing
  , optionValueStringMaxLen         = Nothing
  }

optionCh :: Text -> Text -> OptionValue
optionCh name description = OptionValueChannel
  { optionValueName                 = name
  , optionValueLocalizedName        = Nothing
  , optionValueDescription          = description
  , optionValueRequired             = False
  , optionValueLocalizedDescription = Nothing
  , optionValueChannelTypes         = Just [ApplicationCommandChannelTypeGuildText]
  }

optionRole :: Text -> Text -> OptionValue
optionRole name description = OptionValueRole
  { optionValueName                 = name
  , optionValueLocalizedName        = Nothing
  , optionValueDescription          = description
  , optionValueRequired             = False
  , optionValueLocalizedDescription = Nothing
  }

requiredOpt :: (Text -> Text -> OptionValue) -> Text -> Text -> OptionValue
requiredOpt builder name description =
  (builder name description) { optionValueRequired = True }

required :: (Text -> Maybe OptionsData -> DiscordHandler (Maybe a)) -> Text -> Maybe OptionsData -> DiscordHandler a
required getter name opts = getter name opts <&> \case
  Just a -> a
  Nothing -> error "Asked for required option but it wasn't registered as such."

getStrOpt :: Text -> Maybe OptionsData -> DiscordHandler (Maybe Text)
getStrOpt = getOpt toStr where
  toStr (OptionDataValueString _ (Right str)) = Just str
  toStr _                                     = Nothing

getChOpt :: Text -> Maybe OptionsData -> DiscordHandler (Maybe ChannelId)
getChOpt = getOpt toCh where
  toCh (OptionDataValueChannel _ cId) = Just cId
  toCh _                              = Nothing

getRoleOpt :: Text -> Maybe OptionsData -> DiscordHandler (Maybe RoleId)
getRoleOpt = getOpt toRole where
  toRole (OptionDataValueRole _ rId) = Just rId
  toRole _                           = Nothing

getOpt :: (OptionDataValue -> Maybe a) -> Text -> Maybe OptionsData -> DiscordHandler (Maybe a)
getOpt toA name (opt name toA -> a) = pure a

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
notificationChCmd name thingToNotify setter = slash $ SlashProps
  { name    = name
  , desc    = "Sets or clears the channel in which to send " <> thingToNotify <> " notifications"
  , permLvl = PermLvlBotManager
  , options =
      [ optionCh "channel" $
          "The channel (if any) in which to send " <> thingToNotify <> " notifications"
      ]
  , handler = \db intr _mem guildId opts -> do
      chanId <- getChOpt "channel" opts
      
      changeSettings db guildId $ setter (w64 <$> chanId)

      replyEmbed intr $ case chanId of
        Nothing -> "Successfully disabled notifications for " <> thingToNotify <> "s."
        Just id -> "Successfully set <#" <> show id <> "> as the notification channel for " <> thingToNotify <> "s."
  }

roleCmd :: Text -> Text -> IsOptionRequired -> (Maybe Word64 -> GuildSettings -> GuildSettings) -> SlashCommand
roleCmd name purpose isRequired setter = slash $ SlashProps
  { name    = name
  , desc    = "Sets the role which " <> purpose
  , permLvl = PermLvlBotManager
  , options =
      [ maybeRequired isRequired optionRole "role" $ "The role which " <> purpose
      ]
  , handler = \db intr _mem guildId opts -> do
      roleId <- getRoleOpt "role" opts
      
      changeSettings db guildId $ setter (w64 <$> roleId)

      replyEmbed intr $ case roleId of
        Nothing -> "Successfully unset the role which " <> purpose <> "."
        Just id -> "Successfully set <@&" <> show id <> "> as the role which "<> purpose <>"."
  }

data IsOptionRequired
  = Required
  | Optional

maybeRequired :: IsOptionRequired -> (Text -> Text -> OptionValue) -> Text -> Text -> OptionValue
maybeRequired Required = requiredOpt
maybeRequired Optional = identity
