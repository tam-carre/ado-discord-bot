{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module DiscordBot.Guilds.Settings
  ( GuildSettings (..)
  , SettingsDb
  , getSettings
  , getAllSettings
  , getSettingsDb
  , changeSettings
  , dId
  , w64
  ) where

-- Ado Bot modules
import Utils (tap)

-- Downloaded libraries
import Data.SafeCopy (deriveSafeCopy, base)
import Discord.Types (Snowflake (unSnowflake), DiscordId (unId))

import Data.Acid
  ( Update
  , Query
  , makeAcidic
  , openLocalState
  , update
  , query
  , AcidState
  )

-- Base
import Control.Exception (handle)

import qualified Data.Map.Lazy as Map

-------------------------------------------------------------------------------

-- | I don't know how to make DiscordId serializable for Acid State
-- so we have to unwrap the DiscordIds into Word64
data GuildSettings = GuildSettings
  { communityPostCh   :: Maybe Word64
  , secretBaseCh      :: Maybe Word64
  , communityPostRole :: Maybe Word64
  , secretBaseRole    :: Maybe Word64
  , modRole           :: Maybe Word64
  }
$(deriveSafeCopy 0 'base ''GuildSettings)

data SettingsDb = SettingsDb !(Map Word64 GuildSettings)
$(deriveSafeCopy 0 'base ''SettingsDb)

-------------------------------------------------------------------------------

-- Internals (Due to TH they cannot be at the bottom)

findOne :: Word64 -> Query SettingsDb GuildSettings
findOne guildId = byGuildId guildId <$> ask

findAll :: Query SettingsDb (Map Word64 GuildSettings)
findAll = unSettingsDb <$> ask

upsert :: Word64 -> GuildSettings -> Update SettingsDb ()
upsert guildId new = put . setByGuildId guildId new =<< get

byGuildId :: Word64 -> SettingsDb -> GuildSettings
byGuildId guildId (SettingsDb allS) =
  fromMaybe (GuildSettings Nothing Nothing Nothing Nothing Nothing)
    $ Map.lookup guildId allS

unSettingsDb :: SettingsDb -> Map Word64 GuildSettings
unSettingsDb (SettingsDb theMap) = theMap

setByGuildId :: Word64 -> GuildSettings -> SettingsDb -> SettingsDb
setByGuildId guildId new (SettingsDb allS) = SettingsDb $ Map.insert guildId new allS

$(makeAcidic ''SettingsDb ['findOne, 'upsert, 'findAll])

------------------------------------------------------------------------------

-- Exported bindings

-- | Get the settings DB. Should be done once per runtime.
getSettingsDb :: IO (AcidState SettingsDb)
getSettingsDb = openLocalState (SettingsDb Map.empty)
  & tap (\_ -> echo "Successfully connected to the settings DB")
  & handle (\e -> die $ "Problem accessing DB: " <> show (e :: SomeException))

-- | Get a guild's settings
getSettings :: MonadIO m => AcidState SettingsDb -> DiscordId a -> m GuildSettings
getSettings db guildId = liftIO . query db . FindOne $ w64 guildId

-- | Get a guild's settings
getAllSettings :: MonadIO m => AcidState SettingsDb -> m (Map Word64 GuildSettings)
getAllSettings db = liftIO . query db $ FindAll

-- | Unwrap DiscordId into Word64
w64 :: DiscordId a -> Word64
w64 = unSnowflake . unId

-- | Unwrap DiscordId into Just Word64
dId :: DiscordId a -> Maybe Word64
dId = Just . w64

-- | Apply a function over a guild's settings
changeSettings :: MonadIO m
  => AcidState SettingsDb
  -> DiscordId a
  -> (GuildSettings -> GuildSettings)
  -> m ()
changeSettings db guildId f = liftIO $ do
  old <- getSettings db guildId
  update db $ Upsert (w64 guildId) (f old)
