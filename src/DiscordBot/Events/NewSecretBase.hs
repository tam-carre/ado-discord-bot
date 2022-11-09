{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.NewSecretBase (onNewSecretBase) where

-- Ado Bot modules
import Lenses
import DiscordBot.Events.Notify   (notify, Notif (..))
import DiscordBot.Guilds.Settings (SettingsDb, GuildSettings (..))
import Notifications.SecretBase   (SecretBaseLive (..))

-- Downloaded libraries
import Discord   (DiscordHandler)
import Data.Acid (AcidState)

-------------------------------------------------------------------------------

onNewSecretBase :: AcidState SettingsDb -> SecretBaseLive -> DiscordHandler ()
onNewSecretBase settingsDb live = notify settingsDb Notif
  { _settingsToCh   = _secretBaseCh
  , _settingsToRole = _secretBaseRole
  , _nThumb         = Just $ live^.thumb
  , _nAuthor        = live^.title
  , _embedContent   = live^.desc
  , _embedUrl       = live^.url
  , _msgTxt         = Just "Ado is live on Secret Base!"
  }
