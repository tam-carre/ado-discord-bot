{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module DiscordBot.Guilds.Settings
  ( GuildSettings (..)
  , getSettings
  , changeSettings
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
import qualified Data.Map.Lazy as Map

-------------------------------------------------------------------------------

data GuildSettings = GuildSettings
  { communityPostChannels :: Maybe Word64
  , secretBaseChannels    :: Maybe Word64
  , communityPostRole     :: Maybe Word64
  , secretBaseRole        :: Maybe Word64
  }
$(deriveSafeCopy 0 'base ''GuildSettings)

newtype Db = Db (Map Word64 GuildSettings)
$(deriveSafeCopy 0 'base ''Db)

findOne :: Word64 -> Query Db GuildSettings
findOne guildId = byGuildId guildId <$> ask

upsert :: Word64 -> GuildSettings -> Update Db ()
upsert guildId new = put . setByGuildId guildId new =<< get

byGuildId :: Word64 -> Db -> GuildSettings
byGuildId guildId (Db allS) =
  fromMaybe (GuildSettings Nothing Nothing Nothing Nothing)
    $ Map.lookup guildId allS

setByGuildId :: Word64 -> GuildSettings -> Db -> Db
setByGuildId guildId new (Db allS) = Db $ Map.insert guildId new allS

$(makeAcidic ''Db ['findOne, 'upsert])

getDb :: IO (AcidState Db)
getDb = openLocalState (Db Map.empty)

getWithDb :: DiscordId a -> AcidState Db -> IO GuildSettings
getWithDb guildId db = query db . FindOne $ w64 guildId

w64 :: DiscordId a -> Word64
w64 = unSnowflake . unId

getSettings :: MonadIO m => DiscordId a -> m GuildSettings
getSettings = liftIO . (getDb >>=) . getWithDb

changeSettings :: MonadIO m => DiscordId a -> (GuildSettings -> GuildSettings) -> m ()
changeSettings guildId f = liftIO $ do
  db  <- getDb
  old <- getWithDb guildId db
  update db $ Upsert (w64 guildId) (f old)
