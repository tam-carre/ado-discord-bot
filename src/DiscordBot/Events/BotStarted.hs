--  Why this handler and not just the Ready event?
--
-- The Discord Gateway docs do not guarantee that Ready is only emitted once.
-- In case of connection issues we do not know whether Discord will re-emit
-- the Ready event. Therefore, tasks which should only be started once per
-- session should be initiated in this handler for discord-haskell's
-- `discordOnStart` property.
--
-- NOTE: Although this returns a `DiscordHandler ()` and hence has access
-- to Gateway requests, `discordOnStart` is actually run before receiving
-- any Ready event. The functionality to ensure tasks are run only once,
-- and only after Ready is fired, is not implemented.
--
-- NOTE2: This handler blocks bot execution until it returns.

{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.BotStarted (onBotStarted) where

-- Ado Bot modules
import App                                (App)
import Utils                              (sleep)
import DiscordBot.Events.NewCommunityPost (onNewCommunityPost)
import DiscordBot.Events.NewSecretBase    (onNewSecretBase)
import DiscordBot.Events.NewYTStream      (onNewYTStream)
import Notifications.YTCommunityPosts     (getNextNewCommunityPost)
import Notifications.SecretBase           (getNextNewSecretBase)

-- Downloaded libraries
import UnliftIO.Concurrent (forkIO)
import Notifications.YTLivestream (getNextNewLivestream)

-------------------------------------------------------------------------------

-- | Good to know: This handler blocks bot execution until it returns.
onBotStarted :: App ()
onBotStarted = do
  echo "Bot started."
  void . forkIO $ do
    sleep 5 -- This may help waiting till the bot is ready
    echo "Starting notifiers."
    watch getNextNewCommunityPost onNewCommunityPost
    watch getNextNewSecretBase onNewSecretBase
    watch getNextNewLivestream onNewYTStream

watch :: App a -> (a -> App ()) -> App ()
watch watcher handler = void . forkIO $ do
  justCameOut <- watcher
  void . forkIO $ handler justCameOut
  sleep 30
  watch watcher handler
