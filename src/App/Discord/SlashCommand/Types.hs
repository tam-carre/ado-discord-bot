{-# LANGUAGE DuplicateRecordFields #-}

module App.Discord.SlashCommand.Types (SlashCommand (..)) where

import App                     (App)
import App.Discord.Perms.Types (PermLvl (..))
import Discord.Interactions    (CreateApplicationCommand (..), Interaction, OptionsData)
import Discord.Types           (GuildId, GuildMember)

----------------------------------------------------------------------------------------------------

type SlashHandler = Interaction → GuildMember → GuildId → Maybe OptionsData → App ()

data SlashCommand
  = SlashCommand
    { _name    ∷ Text
    , _desc    ∷ Text
    , _permLvl ∷ PermLvl
    , _reg     ∷ Maybe CreateApplicationCommand
    , _handler ∷ SlashHandler
    }
