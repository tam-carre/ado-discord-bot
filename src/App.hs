module App (App, Db (..)) where

-- Ado Bot modules
import Notifications.History      (NotifHistoryDb)
import DiscordBot.Guilds.Settings (SettingsDb)

-- Downloaded libraries
import Data.Acid (AcidState)
import Discord   (DiscordHandler)

-------------------------------------------------------------------------------

data Db = Db
  { notifDb    :: AcidState NotifHistoryDb
  , settingsDb :: AcidState SettingsDb
  }

type App a = ReaderT Db DiscordHandler a
