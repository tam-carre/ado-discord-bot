{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module DiscordBot.Guilds.Settings
  ( GuildSettings (..)
  , SettingsDb (..)
  , getSettings
  , getAllSettings
  , getSettingsDb
  , changeSettings
  , dId
  , w64
  , w64DId
  ) where

-- Ado Bot modules
import App   (App, Env (settingsDb))
import Utils (tap)
import DiscordBot.Guilds.Settings.Internal
  ( SettingsDb (..)
  , GuildSettings (..)
  , FindAll (..)
  , FindOne (..)
  , Upsert (..)
  )

-- Downloaded libraries
import Discord.Types (Snowflake (..), DiscordId (..))
import Data.Acid
  ( openLocalState
  , update
  , query
  , AcidState
  )

-- Base
import Control.Exception (handle)
import qualified Data.Map.Lazy as Map

------------------------------------------------------------------------------

-- | Get the settings DB. Should be done once per runtime.
getSettingsDb :: IO (AcidState SettingsDb)
getSettingsDb = openLocalState (SettingsDb Map.empty)
  & tap (\_ -> echo "Successfully connected to the settings DB")
  & handle (\e -> die $ "Problem accessing DB: " <> show (e :: SomeException))

-- | Get a guild's settings
getSettings :: DiscordId a -> App GuildSettings
getSettings guildId = do
  db <- asks settingsDb
  liftIO . query db . FindOne $ w64 guildId

-- | Get a guild's settings
getAllSettings :: MonadIO m => AcidState SettingsDb -> m (Map Word64 GuildSettings)
getAllSettings db = liftIO . query db $ FindAll

-- | Unwrap DiscordId into Word64
w64 :: DiscordId a -> Word64
w64 = unSnowflake . unId

-- | Unwrap DiscordId into Just Word64
dId :: DiscordId a -> Maybe Word64
dId = Just . w64

w64DId :: Word64 -> DiscordId a
w64DId = DiscordId . Snowflake

-- | Apply a function over a guild's settings
changeSettings :: DiscordId a -> (GuildSettings -> GuildSettings) -> App ()
changeSettings guildId f = do
  db <- asks settingsDb
  old <- getSettings guildId
  liftIO . update db $ Upsert (w64 guildId) (f old)
