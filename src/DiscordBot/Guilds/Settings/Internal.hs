{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE LambdaCase         #-}

module DiscordBot.Guilds.Settings.Internal where

-- Downloaded libraries
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Acid     (Update, Query, makeAcidic)

-- Base
import qualified Data.Map.Lazy as Map

-------------------------------------------------------------------------------

-- | I don't know how to make DiscordId serializable for Acid State
-- so we have to unwrap the DiscordIds into Word64
data GuildSettings = GuildSettings
  { _communityPostCh   :: Maybe Word64
  , _secretBaseCh      :: Maybe Word64
  , _communityPostRole :: Maybe Word64
  , _secretBaseRole    :: Maybe Word64
  , _modRole           :: Maybe Word64
  , _relayCh           :: Maybe Word64
  }
$(deriveSafeCopy 0 'base ''GuildSettings)

newtype SettingsDb = SettingsDb (Map Word64 GuildSettings)
$(deriveSafeCopy 0 'base ''SettingsDb)

findOne :: Word64 -> Query SettingsDb GuildSettings
findOne guildId = byGuildId guildId <$> ask

findAll :: Query SettingsDb (Map Word64 GuildSettings)
findAll = unSettingsDb <$> ask

upsert :: Word64 -> GuildSettings -> Update SettingsDb ()
upsert guildId new = put . setByGuildId guildId new =<< get

byGuildId :: Word64 -> SettingsDb -> GuildSettings
byGuildId guildId (SettingsDb allS) =
  fromMaybe (GuildSettings Nothing Nothing Nothing Nothing Nothing Nothing)
    $ Map.lookup guildId allS

unSettingsDb :: SettingsDb -> Map Word64 GuildSettings
unSettingsDb (SettingsDb theMap) = theMap

setByGuildId :: Word64 -> GuildSettings -> SettingsDb -> SettingsDb
setByGuildId guildId new (SettingsDb allS) = SettingsDb $ Map.insert guildId new allS

$(makeAcidic ''SettingsDb ['findOne, 'upsert, 'findAll])
