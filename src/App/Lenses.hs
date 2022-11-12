-- | This module centralizes lenses (for convenience &
-- so that they may share the same name)
-- or to produce lenses for library datatypes
-- To debug: {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, FunctionalDependencies, TemplateHaskell #-}

module App.Lenses (module App.Lenses, module Control.Lens) where

import App.BotConfig                               (BotConfig (..))
import App.Discord.Events.NewYTChatMsg.Internal    (Msg (..))
import App.Discord.Events.Notify.Types             (Notif (..))
import App.Discord.Guilds.Settings.Internal        (GuildSettings (..))
import App.Discord.SlashCommand.Types              (SlashCommand (..), SlashProps (..))
import App.Notifications.History                   (NotifHistoryDb (..))
import App.Notifications.SecretBase.Internal       (SecretBaseLive (..), SecretBaseVid (..))
import App.Notifications.YTCommunityPosts.Internal (CommunityPost (..))
import App.Utils                                   (makeFieldsOptionalPrefix)
import Control.Lens
import Discord
import Discord.Interactions
import Discord.Requests
import Discord.Types

----------------------------------------------------------------------------------------------------

makeFields ''Interaction

makeLensesWith abbreviatedFields ''GuildMember
makeLensesWith abbreviatedFields ''CreateApplicationCommand
makeLensesWith abbreviatedFields ''RunDiscordOpts

makeFields ''CreateEmbed
makeFields ''OptionValue
makeFields ''GatewayIntent
makeFields ''InteractionResponseMessage
makeFields ''ApplicationCommand
makeFields ''OptionDataValue
makeFieldsOptionalPrefix "messageDetailed" ''MessageDetailedOpts
makeFieldsOptionalPrefix "applicationCommandData" ''ApplicationCommandData

makeFieldsNoPrefix ''SecretBaseLive
makeFieldsNoPrefix ''SecretBaseVid
makeFieldsNoPrefix ''BotConfig
makeFieldsNoPrefix ''GuildSettings
makeFieldsNoPrefix ''Notif
makeFieldsNoPrefix ''NotifHistoryDb
makeFieldsNoPrefix ''CommunityPost
makeFieldsNoPrefix ''SlashCommand
makeFieldsNoPrefix ''SlashProps
makeFieldsNoPrefix ''Msg

(≫^.) ∷ Functor f ⇒ f a → Getting b a b → f b
(≫^.) q g = q <&> (^. g)

(>>^.) ∷ Functor f ⇒ f a → Getting b a b → f b
(>>^.) = (≫^.)
