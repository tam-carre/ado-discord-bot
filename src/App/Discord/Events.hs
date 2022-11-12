{-# LANGUAGE LambdaCase #-}

module App.Discord.Events (onDiscordEvent) where

import App                                          (App)
import App.Discord.Events.Gateway.InteractionCreate (onInteractionCreate)
import App.Discord.Events.Gateway.Ready             (onReady)
import Discord.Types                                (Event (..), PartialApplication (..))

----------------------------------------------------------------------------------------------------

-- | Good to know:
-- [1] If an event handler throws, discord-haskell will carry on.
-- [2] onDiscordEvent/discordOnEvent is run in a thread
-- [3] Unsure what the Event data constructors' parameters are? Check out
--     <https://discord.com/developers/docs/topics/gateway-events#receive-events>
--     Parameters in discord-haskell are in the same order as in the above
--     documentation.
onDiscordEvent ∷ Event → App ()
onDiscordEvent = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) → onReady appId
  InteractionCreate intr                         → onInteractionCreate intr
  _                                              → pass
