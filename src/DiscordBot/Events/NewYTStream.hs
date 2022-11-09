{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.NewYTStream (onNewYTStream) where

-- Ado Bot modules
import DiscordBot.Events.NewYTChatMsg (onNewYTChatMsg)
import DiscordBot.Events.Notify       (notify, Notif (..))
import Notifications.YTLivestream     (VideoId)
import DiscordBot.Guilds.Settings     (GuildSettings (..))

-- Downloaded libraries
import System.Process           (shell)
import System.Process.Streaming (execute, piped, foldOutErr)
import qualified System.Process.Streaming.Text as PT
import App (App)

-------------------------------------------------------------------------------

onNewYTStream :: VideoId -> App ()
onNewYTStream vidId = do
  let url = "https://youtu.be/" <> vidId

  notify Notif
    { _settingsToCh   = _relayCh
    , _settingsToRole = const Nothing
    , _nThumb         = Nothing
    , _nAuthor        = ""
    , _embedContent   = "Ado is live at " <> url <> " ! I will relay live translations here."
    , _embedUrl       = url
    , _msgTxt         = Nothing
    }

  db <- ask
  discordHandle <- lift ask
  let onMsgDiscord line = runReaderT (onNewYTChatMsg line) db
      onMsgIO      line = runReaderT (onMsgDiscord line)   discordHandle

  liftIO $ execute
    (piped . shell $ "node ./masterchat/index.js " <> toString vidId)
    (foldOutErr . PT.bothAsUtf8x . PT.combinedLines . PT.eachLine $ onMsgIO)
