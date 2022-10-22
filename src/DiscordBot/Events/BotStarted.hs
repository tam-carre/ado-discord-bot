-- | Why this handler and not just the Ready event?
-- The Discord Gateway docs do not guarantee that Ready is only emitted once.
-- In case of connection issues we do not know whether Discord will re-emit
-- the Ready event. Therefore, tasks which should only be started once per
-- session should be initiated in this handler for discord-haskell's
-- `discordOnStart` property.
-- NOTE: Although this returns a `DiscordHandler ()` and hence has access
-- to Gateway requests, `discordOnStart` is actually run before receiving
-- any Ready event. The functionality to ensure tasks are run only once,
-- and only after Ready is fired, is not implemented.

{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.BotStarted (onBotStarted) where

-- Downloaded libraries
import Discord (DiscordHandler)

-------------------------------------------------------------------------------

onBotStarted :: DiscordHandler ()
onBotStarted = echo "Bot started."
