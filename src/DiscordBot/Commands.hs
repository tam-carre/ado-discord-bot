module DiscordBot.Commands (appCommands, cmdByName) where

-- Ado Bot modules
import DiscordBot.Commands.Ping (ping)
import DiscordBot.SlashCommand  (SlashCommand (..))

-- Downloaded libraries
import Data.Text (Text)

-- Base
import Data.Foldable (find)

-------------------------------------------------------------------------------

appCommands :: [SlashCommand]
appCommands = [ping]

cmdByName :: Text -> Maybe SlashCommand
cmdByName cmdName = find ((==) cmdName  . name) appCommands
