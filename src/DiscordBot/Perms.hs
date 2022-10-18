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

-- Base
import Data.Functor ((<&>))

-------------------------------------------------------------------------------

data PermLvl
  = PermLvlUser
  | PermLvlMod
  | PermLvlBotOwner
  deriving (Eq, Ord, Show)

getPermLvl :: GuildMember -> PermLvl
getPermLvl member
  | isBotOwner member = PermLvlBotOwner
  -- | isMod member      = PermLvlMod
  | otherwise         = PermLvlUser

isBotOwner :: GuildMember -> Bool
isBotOwner member = memberId == ownerId where
  memberId = member.memberUser <&> userId
  ownerId  = Just . DiscordId $ Snowflake botConfig.ownerUserId

-- TODO: Implement permission parsing?
-- isMod :: GuildMember -> Bool
-- isMod member = undefined
