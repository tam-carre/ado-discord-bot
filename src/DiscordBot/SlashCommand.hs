{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DiscordBot.SlashCommand
  ( slash
  , optionString
  , SlashCommand (..)
  , SlashProps (..)
  ) where

-- Downloaded libraries
import Discord       (DiscordHandler)
import Discord.Types (GuildMember, InteractionId, InteractionToken)

import Discord.Interactions
  ( CreateApplicationCommand (..)
  , createChatInput
  , Options (OptionsValues)
  , OptionValue (..)
  , OptionsData
  )
import DiscordBot.Perms (PermLvl)

-------------------------------------------------------------------------------

data SlashCommand = SlashCommand
  { name         :: Text
  , desc         :: Text
  , permLvl      :: PermLvl
  , options      :: [OptionValue]
  , handler      :: (InteractionId, InteractionToken)
                 -> GuildMember
                 -> Maybe OptionsData
                 -> DiscordHandler ()
  , registration :: Maybe CreateApplicationCommand
  }

data SlashProps = SlashProps
  { name    :: Text
  , desc    :: Text
  , permLvl :: PermLvl
  , options :: [OptionValue]
  , handler :: (InteractionId, InteractionToken)
            -> GuildMember
            -> Maybe OptionsData
            -> DiscordHandler ()
  }

slash :: SlashProps -> SlashCommand
slash (SlashProps name desc permLvl options handler) = SlashCommand
  { name
  , desc
  , permLvl
  , options
  , handler
  , registration =
    createChatInput name desc >>= \case
      slashCmd@CreateApplicationCommandChatInput {} ->
          Just $ slashCmd { createOptions = Just $ OptionsValues options }

      _ -> Nothing
  }

optionString :: Text -> Text -> OptionValue
optionString name description = OptionValueString
  { optionValueName                 = name
  , optionValueLocalizedName        = Nothing
  , optionValueDescription          = description
  , optionValueLocalizedDescription = Nothing
  , optionValueRequired             = True
  , optionValueStringChoices        = Left False
  , optionValueStringMinLen         = Nothing
  , optionValueStringMaxLen         = Nothing
  }
