module App (App, Env (..)) where

-- Ado Bot modules
import Notifications.History.Internal      (NotifHistoryDb)
import DiscordBot.Guilds.Settings.Internal (SettingsDb)

-- Downloaded libraries
import Data.Acid (AcidState)
import Discord   (DiscordHandler)

-------------------------------------------------------------------------------

data Env = Env
  { notifDb    :: AcidState NotifHistoryDb
  , settingsDb :: AcidState SettingsDb
  }

type App a = ReaderT Env DiscordHandler a
