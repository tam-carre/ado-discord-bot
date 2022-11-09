{-# LANGUAGE DuplicateRecordFields #-}

module DiscordBot.SlashCommand.Types (SlashCommand (..), SlashProps (..)) where

-- Ado Bot modules
import DiscordBot.Perms.Types     (PermLvl (..))
import DiscordBot.Guilds.Settings (SettingsDb)

-- Downloaded libraries
import Discord       (DiscordHandler)
import Data.Acid     (AcidState)
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
  , _handler      :: AcidState SettingsDb
                  -> (InteractionId, InteractionToken)
                  -> GuildMember
                  -> GuildId
                  -> Maybe OptionsData
                  -> DiscordHandler ()
  , _registration :: Maybe CreateApplicationCommand
  }

data SlashProps = SlashProps
  { _name    :: Text
  , _desc    :: Text
  , _permLvl :: PermLvl
  , _options :: [OptionValue]
  , _handler :: AcidState SettingsDb
             -> (InteractionId, InteractionToken)
             -> GuildMember
             -> GuildId
             -> Maybe OptionsData
             -> DiscordHandler ()
  }

