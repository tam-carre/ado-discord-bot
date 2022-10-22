module DiscordBot.Commands (appCommands, cmdByName) where

-- Ado Bot modules
import DiscordBot.Commands.Ping (ping)
import DiscordBot.SlashCommand  (SlashCommand (..))

-------------------------------------------------------------------------------

appCommands :: [SlashCommand]
appCommands = [ping]

cmdByName :: Text -> Maybe SlashCommand
cmdByName cmdName = find ((==) cmdName  . name) appCommands
