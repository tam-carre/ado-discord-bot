module DiscordBot.Events.Notify.Types (Notif (..)) where

import DiscordBot.Guilds.Settings (GuildSettings)

-------------------------------------------------------------------------------

data Notif = Notif
  { _settingsToCh   :: GuildSettings -> Maybe Word64
  , _settingsToRole :: GuildSettings -> Maybe Word64
  , _nThumb         :: Maybe Text
  , _nAuthor        :: Text
  , _embedContent   :: Text
  , _embedUrl       :: Text
  , _msgTxt         :: Maybe Text
  }
