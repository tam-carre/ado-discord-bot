{-# LANGUAGE FieldSelectors #-}

module App (App, Env (..)) where

import App.Discord.Guilds.Settings.Internal (SettingsDb)
import App.Notifications.History.Internal   (NotifHistoryDb)
import Data.Acid                            (AcidState)
import Discord                              (DiscordHandler)

----------------------------------------------------------------------------------------------------

data Env
  = Env
    { notifDb    ∷ AcidState NotifHistoryDb
    , settingsDb ∷ AcidState SettingsDb
    }

type App a = ReaderT Env DiscordHandler a
