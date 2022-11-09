{-# LANGUAGE DuplicateRecordFields #-}

module DiscordBot.SlashCommand.Types (SlashCommand (..), SlashProps (..)) where

-- Ado Bot modules
import App                    (App)
import DiscordBot.Perms.Types (PermLvl (..))

-- Downloaded libraries
import Discord.Types (GuildMember, InteractionId, InteractionToken, GuildId)
import Discord.Interactions
  ( CreateApplicationCommand (..)
  , OptionValue (..)
  , OptionsData
  )

-------------------------------------------------------------------------------

data SlashCommand = SlashCommand
  { _name         :: Text
  , _desc         :: Text
  , _permLvl      :: PermLvl
  , _options      :: [OptionValue]
  , _handler      :: (InteractionId, InteractionToken)
                  -> GuildMember
                  -> GuildId
                  -> Maybe OptionsData
                  -> App ()
  , _registration :: Maybe CreateApplicationCommand
  }

data SlashProps = SlashProps
  { _name    :: Text
  , _desc    :: Text
  , _permLvl :: PermLvl
  , _options :: [OptionValue]
  , _handler :: (InteractionId, InteractionToken)
             -> GuildMember
             -> GuildId
             -> Maybe OptionsData
             -> App ()
  }

