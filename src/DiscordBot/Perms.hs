{-# LANGUAGE OverloadedRecordDot #-}

module DiscordBot.Perms (PermLvl (..), getPermLvl) where

-- Ado Bot modules
import Config.BotConfig           (botConfig)
import Config.Type                (BotConfig (..))
import DiscordBot.Guilds.Settings (GuildSettings (..), w64)

-- Downloaded libraries
import Discord.Types
  ( GuildMember (..)
  , User (..)
  , DiscordId (..)
  , Snowflake (..)
  )

-------------------------------------------------------------------------------

data PermLvl
  = PermLvlUser
  | PermLvlBotManager
  | PermLvlBotOwner
  deriving (Eq, Ord, Show)

getPermLvl :: GuildSettings -> GuildMember -> PermLvl
getPermLvl g member
  | isBotOwner member     = PermLvlBotOwner
  | isBotManager g member = PermLvlBotManager
  | otherwise              = PermLvlUser

isBotOwner :: GuildMember -> Bool
isBotOwner member = memberId == ownerId where
  memberId = member.memberUser <&> userId
  ownerId  = Just . DiscordId $ Snowflake botConfig.ownerUserId

isBotManager :: GuildSettings -> GuildMember -> Bool
isBotManager g member = isJust g.modRole && memberId == g.modRole where
  memberId = member.memberUser <&> userId <&> w64
