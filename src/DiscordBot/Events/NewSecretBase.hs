{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.NewSecretBase (onNewSecretBase) where

-- Ado Bot modules
import Lenses
import App                        (App)
import DiscordBot.Events.Notify   (notify, Notif (..))
import DiscordBot.Guilds.Settings (GuildSettings (..))
import Notifications.SecretBase   (SecretBaseLive (..))

-------------------------------------------------------------------------------

onNewSecretBase :: SecretBaseLive -> App ()
onNewSecretBase live = notify Notif
  { _settingsToCh   = _secretBaseCh
  , _settingsToRole = _secretBaseRole
  , _nThumb         = Just $ live^.thumb
  , _nAuthor        = live^.title
  , _embedContent   = live^.desc
  , _embedUrl       = live^.url
  , _msgTxt         = Just "Ado is live on Secret Base!"
  }
