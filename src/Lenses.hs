-- | This module centralizes lenses (for convenience &
-- so that they may share the same name)
-- or to produce lenses for library datatypes
-- To debug: {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}

module Lenses (module Lenses, module Control.Lens) where

-- Ado Bot modules
import Utils                                   (makeFieldsOptionalPrefix)
import BotConfig                               (BotConfig (..))
import Notifications.History                   (NotifHistoryDb (..))
import Notifications.SecretBase.Internal       (SecretBaseLive (..))
import Notifications.YTCommunityPosts.Internal (CommunityPost (..))
import DiscordBot.Guilds.Settings              (GuildSettings (..))
import DiscordBot.Events.Notify.Types          (Notif (..))
import DiscordBot.SlashCommand.Types           (SlashCommand (..), SlashProps (..))
import App.Types                               (Db (..))

-- Downloaded libraries
import Control.Lens -- Not sure if reexporting everything is a good idea, we'll see
import Discord
import Discord.Types
import Discord.Interactions
import Discord.Requests

-------------------------------------------------------------------------------

makeFields ''Interaction

makeLensesWith abbreviatedFields ''GuildMember
makeLensesWith abbreviatedFields ''SecretBaseLive
makeLensesWith abbreviatedFields ''CreateApplicationCommand
makeLensesWith abbreviatedFields ''RunDiscordOpts

makeFields ''CreateEmbed
makeFields ''OptionValue
makeFields ''GatewayIntent
makeFields ''InteractionResponseMessage
makeFields ''ApplicationCommand
makeFieldsOptionalPrefix "messageDetailed" ''MessageDetailedOpts
makeFieldsOptionalPrefix "applicationCommandData" ''ApplicationCommandData

makeFieldsNoPrefix ''BotConfig
makeFieldsNoPrefix ''GuildSettings
makeFieldsNoPrefix ''Notif
makeFieldsNoPrefix ''NotifHistoryDb
makeFieldsNoPrefix ''CommunityPost
makeFieldsNoPrefix ''SlashCommand
makeFieldsNoPrefix ''SlashProps
makeFieldsNoPrefix ''Db
