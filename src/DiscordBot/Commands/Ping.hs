{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Commands.Ping (ping) where

-- Ado Bot modules
import DiscordBot.Perms       (PermLvl (..))
import DiscordBot.SendMessage (replyEmbed)

import DiscordBot.SlashCommand
  ( slash
  , SlashProps (..)
  , optionString
  , SlashCommand
  )

-- Downloaded libraries
import Discord.Interactions (OptionsData (..), OptionDataValue (..))

-------------------------------------------------------------------------------

ping :: SlashCommand
ping = slash $ SlashProps
  { name = "ping"
  , desc = "Sends an input back"
  , permLvl = PermLvlUser
  , options = [ optionString "input" "The text to send back" ]
  , handler =
      \(iId, iToken) _mem opts ->
        case opts of
          Just (OptionsDataValues vals) -> do
            case find ((==) "input" . optionDataValueName) vals of
              Just inputOption ->
                case optionDataValueString inputOption of
                  Right input -> replyEmbed (iId, iToken) input
                  Left err -> putTextLn $ "Misconfigured command: " <> err

              _ -> putTextLn "Misconfigured command"

          _ -> putTextLn "Misconfigured command"
  }
