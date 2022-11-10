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
{-# LANGUAGE LambdaCase        #-}

module DiscordBot.Events.BotStartedStartNotifiers
  ( onBotStartedStartNotifiers
  ) where

-- Ado Bot modules
import Lenses
import App                            (App)
import Utils                          (sleep)
import DiscordBot.Events.Notify       (Notif (..), notify)
import DiscordBot.Events.NewYTChatMsg (onNewYTChatMsg)
import Notifications.YTCommunityPosts (getNextNewCommunityPost, CommunityPost)
import Notifications.YTLivestream     (getNextNewLivestream, VideoId)
import Notifications.SecretBase
  ( getNextNewSecretBaseStream
  , getNextNewSecretBaseVid
  , SecretBaseLive
  , SecretBaseVid
  )

-- Downloaded libraries
import UnliftIO.Concurrent      (forkIO)
import System.Process           (shell)
import System.Process.Streaming (execute, piped, foldOutErr)
import qualified System.Process.Streaming.Text as PT
import Deepl (translate)

-------------------------------------------------------------------------------

-- | Good to know: This handler blocks bot execution until it returns.
onBotStartedStartNotifiers :: App ()
onBotStartedStartNotifiers = do
  echo "Bot started."
  void . forkIO $ do
    sleep 5 -- This may help waiting till the bot is ready
    echo "Starting notifiers."
    watch getNextNewCommunityPost    onNewCommunityPost
    watch getNextNewSecretBaseStream onNewSecretBaseStream
    watch getNextNewSecretBaseVid    onNewSecretBaseVid
    watch getNextNewLivestream       onNewYTStream

watch :: App a -> (a -> App ()) -> App ()
watch watcher handler' = void . forkIO $ do
  justCameOut <- watcher
  void . forkIO $ handler' justCameOut
  sleep 30
  watch watcher handler'


-- "Something just came out" handlers

onNewCommunityPost :: CommunityPost -> App ()
onNewCommunityPost post = do
  notify Notif
    { _settingsToCh   = view communityPostCh
    , _settingsToRole = view communityPostRole
    , _nThumb         = Just $ post^.avatar
    , _nAuthor        = post^.author
    , _embedContent   = post^.content
    , _embedUrl       = "https://youtube.com/post/" <> (post^.id)
    , _msgTxt         = Just $ "Ado has just published a community post! \n"
                     <> "<https://youtube.com/post/" <> (post^.id) <> ">"
    }

  post^.content & translate >>= \case
    Left err -> echo err
    Right tl -> notify Notif
      { _settingsToCh   = view communityPostCh
      , _settingsToRole = const Nothing
      , _nThumb         = Nothing
      , _nAuthor        = ""
      , _embedContent   = "*[DeepL]* " <> tl
      , _embedUrl       = "https://youtube.com/post/" <> (post^.id)
      , _msgTxt         = Nothing
      }

onNewSecretBaseStream :: SecretBaseLive -> App ()
onNewSecretBaseStream live = notify Notif
  { _settingsToCh   = view secretBaseCh
  , _settingsToRole = view secretBaseRole
  , _nThumb         = Just $ live^.thumb
  , _nAuthor        = live^.title
  , _embedContent   = live^.desc
  , _embedUrl       = live^.url
  , _msgTxt         = Just "Ado is live on Secret Base!"
  }

onNewSecretBaseVid :: SecretBaseVid -> App ()
onNewSecretBaseVid vid = notify Notif
  { _settingsToCh   = view secretBaseCh
  , _settingsToRole = view secretBaseRole
  , _nThumb         = Just (vid^.thumb)
  , _nAuthor        = vid^.title
  , _embedContent   = vid^.desc
  , _embedUrl       = vid^.url
  , _msgTxt         = Just "Ado has just uploaded a video on Secret Base!"
  }

onNewYTStream :: VideoId -> App ()
onNewYTStream vidId = do
  let link = "https://youtu.be/" <> vidId

  notify Notif
    { _settingsToCh   = view relayCh
    , _settingsToRole = const Nothing
    , _nThumb         = Nothing
    , _nAuthor        = ""
    , _embedContent   = "Ado is live at " <> link <> " ! I will relay live translations here."
    , _embedUrl       = link
    , _msgTxt         = Nothing
    }

  db <- ask
  discordHandle <- lift ask
  let onMsgDiscord line = runReaderT (onNewYTChatMsg line) db
      onMsgIO      line = runReaderT (onMsgDiscord line)   discordHandle

  liftIO $ execute
    (piped . shell $ "node ./masterchat/index.js " <> toString vidId)
    (foldOutErr . PT.bothAsUtf8x . PT.combinedLines . PT.eachLine $ onMsgIO)
