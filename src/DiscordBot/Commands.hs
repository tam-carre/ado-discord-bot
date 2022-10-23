module DiscordBot.Commands (appCommands, cmdByName) where

-- Ado Bot modules
import DiscordBot.Commands.Ping      (pingCmd)
import DiscordBot.Commands.Community (communityCmd)
import DiscordBot.Commands.ModRole   (modRoleCmd)
import DiscordBot.SlashCommand       (SlashCommand (..))

-------------------------------------------------------------------------------

appCommands :: [SlashCommand]
appCommands = [pingCmd, communityCmd, modRoleCmd]

cmdByName :: Text -> Maybe SlashCommand
cmdByName cmdName = find ((==) cmdName  . name) appCommands
