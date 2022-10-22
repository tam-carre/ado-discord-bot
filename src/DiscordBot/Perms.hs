{-# LANGUAGE OverloadedRecordDot #-}

module DiscordBot.Perms (PermLvl (..), getPermLvl) where

-- Ado Bot modules
import Config.BotConfig (botConfig)
import Config.Type      (BotConfig(..))

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
  | PermLvlBotOwner
  deriving (Eq, Ord, Show)

getPermLvl :: GuildMember -> PermLvl
getPermLvl member
  | isBotOwner member = PermLvlBotOwner
  | otherwise         = PermLvlUser

isBotOwner :: GuildMember -> Bool
isBotOwner member = memberId == ownerId where
  memberId = member.memberUser <&> userId
  ownerId  = Just . DiscordId $ Snowflake botConfig.ownerUserId
