{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.NewYTStream (onNewYTStream) where

-- Ado Bot modules
import DiscordBot.Events.NewYTChatMsg (onNewYTChatMsg)
import DiscordBot.Events.Notify       (notify, Notif (..))
import Notifications.YTLivestream     (VideoId)
import DiscordBot.Guilds.Settings     (SettingsDb, GuildSettings (..))

-- Downloaded libraries
import Discord                  (DiscordHandler)
import Data.Acid                (AcidState)
import System.Process           (shell)
import System.Process.Streaming (execute, piped, foldOutErr)
import qualified System.Process.Streaming.Text as PT

-------------------------------------------------------------------------------

onNewYTStream :: AcidState SettingsDb -> VideoId -> DiscordHandler ()
onNewYTStream db vidId = do
  let url = "https://youtu.be/" <> vidId

  notify db Notif
    { _settingsToCh   = _relayCh
    , _settingsToRole = const Nothing
    , _nThumb         = Nothing
    , _nAuthor        = ""
    , _embedContent   = "Ado is live at " <> url <> " ! I will relay live translations here."
    , _embedUrl       = url
    , _msgTxt         = Nothing
    }

  discordHandle <- ask
  let onMsgIO line = runReaderT (onNewYTChatMsg db line) discordHandle

  liftIO $ execute
    (piped . shell $ "node ./masterchat/index.js " <> toString vidId)
    (foldOutErr . PT.bothAsUtf8x . PT.combinedLines . PT.eachLine $ onMsgIO)
