module App.Types (Db (..)) where

-- Ado Bot modules
import DiscordBot.Guilds.Settings (SettingsDb)

-- Downloaded libraries
import Data.Acid             (AcidState)
import Notifications.History (NotifHistoryDb)

-------------------------------------------------------------------------------

data Db = Db
  { _notifDb    :: AcidState NotifHistoryDb
  , _settingsDb :: AcidState SettingsDb
  }
