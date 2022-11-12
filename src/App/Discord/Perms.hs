module App.Discord.Perms (PermLvl (..), getPermLvl) where

-- Ado Bot modules
import App.BotConfig               (botConfig)
import App.Discord.Guilds.Settings (GuildSettings (..), w64, w64DId)
import App.Discord.Perms.Types     (PermLvl (..))
import App.Lenses                  (_Just, modRole, ownerUserId, roles, user, (^.), (^?))
import Discord.Types               (GuildMember (..), User (..))

----------------------------------------------------------------------------------------------------

getPermLvl ∷ GuildSettings → GuildMember → PermLvl
getPermLvl g member
  | isBotOwner member     = PermLvlBotOwner
  | isBotManager g member = PermLvlBotManager
  | otherwise             = PermLvlUser

isBotOwner ∷ GuildMember → Bool
isBotOwner member = memberId ≡ ownerId where
  memberId = member^?user._Just <&> userId
  ownerId  = Just . w64DId $ botConfig^.ownerUserId

isBotManager ∷ GuildSettings → GuildMember → Bool
isBotManager g member = g^.modRole & maybe False (∈ rolesOfMember) where
  rolesOfMember = member^.roles <&> w64
