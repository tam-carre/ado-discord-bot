{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module DiscordBot.Events.NewSecretBase (onNewSecretBase) where

-- Ado Bot modules
import DiscordBot.Events.Notify   (notify, Notif (..))
import DiscordBot.Guilds.Settings (SettingsDb, GuildSettings (..))
import Notifications.SecretBase   (SecretBaseLive (..))

-- Downloaded libraries
import Discord   (DiscordHandler)
import Data.Acid (AcidState)

-------------------------------------------------------------------------------

onNewSecretBase :: AcidState SettingsDb -> SecretBaseLive -> DiscordHandler ()
onNewSecretBase settingsDb live = notify settingsDb Notif
  { settingsToCh   = secretBaseCh
  , settingsToRole = secretBaseRole
  , nThumb         = Just live.sblThumb
  , nAuthor        = live.sblTitle
  , embedContent   = live.sblDesc
  , embedUrl       = live.sblUrl
  , msgTxt         = Just "Ado is live on Secret Base!"
  }
