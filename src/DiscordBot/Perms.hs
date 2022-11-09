module DiscordBot.Perms (PermLvl (..), getPermLvl) where

-- Ado Bot modules
import Lenses
import BotConfig                  (botConfig)
import DiscordBot.Perms.Types     (PermLvl (..))
import DiscordBot.Guilds.Settings (GuildSettings (..), w64)

-- Downloaded libraries
import Discord.Types
  ( GuildMember (..)
  , User (..)
  , DiscordId (..)
  , Snowflake (..)
  )

-------------------------------------------------------------------------------

getPermLvl :: GuildSettings -> GuildMember -> PermLvl
getPermLvl g member
  | isBotOwner member     = PermLvlBotOwner
  | isBotManager g member = PermLvlBotManager
  | otherwise             = PermLvlUser

isBotOwner :: GuildMember -> Bool
isBotOwner member = memberId == ownerId where
  memberId = member^?user._Just <&> userId
  ownerId  = Just . DiscordId . Snowflake $ botConfig^.ownerUserId

isBotManager :: GuildSettings -> GuildMember -> Bool
isBotManager g member = maybe False (`elem` rolesOfMember) $ g^.modRole where
  rolesOfMember = member^.roles <&> w64
