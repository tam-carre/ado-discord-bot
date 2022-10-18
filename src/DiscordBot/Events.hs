{-# LANGUAGE LambdaCase #-}

module DiscordBot.Events (onDiscordEvent) where

-- Ado Bot modules
import DiscordBot.Events.DiscordAPI.Ready             (onReady)
import DiscordBot.Events.DiscordAPI.InteractionCreate (onInteractionCreate)

-- Downloaded libraries
import Discord (DiscordHandler)

import Discord.Types
  ( Event (..)
  , PartialApplication (..)
  )

-------------------------------------------------------------------------------

-- | Good to know:
-- [1] If an event handler throws, discord-haskell will carry on.
-- [2] onDiscordEvent/discordOnEvent is run in a thread
-- [3] Unsure what the Event data constructors' parameters are? Check out
--     <https://discord.com/developers/docs/topics/gateway-events#receive-events>
--     Parameters in discord-haskell are in the same order as in the above
--     documentation.
onDiscordEvent :: Event -> DiscordHandler ()
onDiscordEvent = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId
  InteractionCreate intr                         -> onInteractionCreate intr
  _                                              -> pure ()
