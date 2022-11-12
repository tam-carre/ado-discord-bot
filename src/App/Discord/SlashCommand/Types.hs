{-# LANGUAGE DuplicateRecordFields #-}

module App.Discord.SlashCommand.Types (SlashCommand (..), SlashProps (..)) where

import App                     (App)
import App.Discord.Perms.Types (PermLvl (..))
import Discord.Interactions    (CreateApplicationCommand (..), OptionValue (..), OptionsData)
import Discord.Types           (GuildId, GuildMember, InteractionId, InteractionToken)

----------------------------------------------------------------------------------------------------

type SlashHandler
  = (InteractionId, InteractionToken) → GuildMember → GuildId → Maybe OptionsData → App ()

data SlashCommand
  = SlashCommand
    { _name         ∷ Text
    , _desc         ∷ Text
    , _permLvl      ∷ PermLvl
    , _options      ∷ [OptionValue]
    , _handler      ∷ SlashHandler
    , _registration ∷ Maybe CreateApplicationCommand
    }

data SlashProps
  = SlashProps
    { _name    ∷ Text
    , _desc    ∷ Text
    , _permLvl ∷ PermLvl
    , _options ∷ [OptionValue]
    , _handler ∷ SlashHandler
    }
