module DiscordBot.Commands (appCommands, cmdByName) where

-- Ado Bot modules
import DiscordBot.Commands.Ping           (pingCmd)
import DiscordBot.Commands.Community      (communityCmd)
import DiscordBot.Commands.ModRole        (modRoleCmd)
import DiscordBot.Commands.SecretBase     (secretBaseCmd)
import DiscordBot.Commands.CommunityRole  (communityRoleCmd)
import DiscordBot.Commands.SecretBaseRole (secretBaseRoleCmd)
import DiscordBot.Commands.Relay          (relayCmd)
import DiscordBot.SlashCommand            (SlashCommand (..))

-------------------------------------------------------------------------------

appCommands :: [SlashCommand]
appCommands =
  [ pingCmd
  , communityCmd
  , secretBaseCmd
  , communityRoleCmd
  , secretBaseRoleCmd
  , modRoleCmd
  , relayCmd
  ]

cmdByName :: Text -> Maybe SlashCommand
cmdByName cmdName = find ((==) cmdName  . _name) appCommands
