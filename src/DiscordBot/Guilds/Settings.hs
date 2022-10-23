{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module DiscordBot.Guilds.Settings
  ( GuildSettings (..)
  , SettingsDb
  , getSettingsWithDb
  , getSettingsDb
  , changeSettings
  , dId
  , w64
  ) where

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

data GuildSettings = GuildSettings
  { communityPostCh       :: Maybe Word64
  , secretBaseCh          :: Maybe Word64
  , communityPostRole     :: Maybe Word64
  , secretBaseRole        :: Maybe Word64
  , modRole               :: Maybe Word64
  }
$(deriveSafeCopy 0 'base ''GuildSettings)

data SettingsDb = SettingsDb !(Map Word64 GuildSettings)
$(deriveSafeCopy 0 'base ''SettingsDb)

findOne :: Word64 -> Query SettingsDb GuildSettings
findOne guildId = byGuildId guildId <$> ask

upsert :: Word64 -> GuildSettings -> Update SettingsDb ()
upsert guildId new = put . setByGuildId guildId new =<< get

byGuildId :: Word64 -> SettingsDb -> GuildSettings
byGuildId guildId (SettingsDb allS) =
  fromMaybe (GuildSettings Nothing Nothing Nothing Nothing Nothing)
    $ Map.lookup guildId allS

setByGuildId :: Word64 -> GuildSettings -> SettingsDb -> SettingsDb
setByGuildId guildId new (SettingsDb allS) = SettingsDb $ Map.insert guildId new allS

$(makeAcidic ''SettingsDb ['findOne, 'upsert])

getSettingsDb :: IO (AcidState SettingsDb)
getSettingsDb = openLocalState (SettingsDb Map.empty)
  & tap (\_ -> echo "Successfully connected to the DB")
  & handle (\e -> die $ "Problem accessing DB: " <> show (e :: SomeException))

getSettingsWithDb :: MonadIO m => AcidState SettingsDb -> DiscordId a -> m GuildSettings
getSettingsWithDb db guildId = liftIO . query db . FindOne $ w64 guildId

w64 :: DiscordId a -> Word64
w64 = unSnowflake . unId

dId :: DiscordId a -> Maybe Word64
dId = Just . w64

changeSettings :: MonadIO m
  => AcidState SettingsDb
  -> DiscordId a
  -> (GuildSettings -> GuildSettings)
  -> m ()
changeSettings db guildId f = liftIO $ do
  old <- getSettingsWithDb db guildId
  update db $ Upsert (w64 guildId) (f old)
