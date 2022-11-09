module App (App) where

-- Ado Bot modules
import App.Types (Db)

-- Downloaded libraries
import Discord   (DiscordHandler)

-------------------------------------------------------------------------------

type App a = ReaderT Db DiscordHandler a
