{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}

module DiscordBot.Events.NewYTStream (onNewYTStream) where

-- Ado Bot modules
import Json                       ((?.), unStr)
import Deepl                      (translate)
import DiscordBot.Events.Notify   (notify, Notif (..))
import DiscordBot.SendMessage     (send')
import Notifications.YTLivestream (VideoId)
import DiscordBot.Guilds.Settings
  ( SettingsDb
  , GuildSettings (..)
  , getAllSettings
  )

-- Downloaded libraries
import Data.Aeson               (Value (..), decode)
import Discord                  (DiscordHandler)
import Data.Acid                (AcidState)
import System.Process           (shell)
import System.Process.Streaming (execute, piped, foldOutErr)
import qualified System.Process.Streaming.Text as PT
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Lazy.Encoding       as Lazy.Encoding
import qualified Data.Map                      as Map

-------------------------------------------------------------------------------

onNewYTStream :: AcidState SettingsDb -> VideoId -> DiscordHandler ()
onNewYTStream db vidId = do
  let url = "https://youtu.be/" <> vidId

  notify db Notif
    { settingsToCh   = relayCh
    , settingsToRole = const Nothing
    , nThumb         = Nothing
    , nAuthor        = ""
    , embedContent   = "Ado is live at " <> url <> " ! I will relay live translations here."
    , embedUrl       = url
    , msgTxt         = Nothing
    }

  discordHandle <- ask
  let processMsgIO line = runReaderT (processMsg db line) discordHandle

  liftIO $ execute
    (piped . shell $ "node ./masterchat/index.js " <> toString vidId)
    (foldOutErr . PT.bothAsUtf8x . PT.combinedLines . PT.eachLine $ processMsgIO)

processMsg :: AcidState SettingsDb -> L.Text -> DiscordHandler (Either Void ())
processMsg db line = do
  allSettings <- getAllSettings db
  let channelsAwaitingMsg  = mapMaybe relayCh $ Map.elems allSettings
  case decode (Lazy.Encoding.encodeUtf8 line) :: Maybe Value of
    Nothing -> err "could not decode msg"
    Just (extract -> Left e) -> err $ "error parsing msg " <> e
    Just (extract -> Right msg)
      | isAdo msg -> do
        echo "Found message by Ado, relaying"
        tl <- rightToMaybe <$> translate msg.content
        forM_ channelsAwaitingMsg $ send' $ adoMsg msg.content tl
        done

      | isTl msg.content -> do
        echo "Found live translation, relaying"
        forM_ channelsAwaitingMsg . send' $ tlMsg msg.name msg.content
        done

      | otherwise ->
        done

    _ -> echo "TODO: not sure what this branch is" >> done
  where
  err x = echo x >> done
  done = pure $ Right ()

isAdo :: Msg -> Bool
isAdo msg = msg.chId == "UCln9P4Qm3-EAY4aiEPmRwEA"

adoMsg :: Text -> Maybe Text -> Text
adoMsg txt tl =
  "<:AdoHappy:885833189086593024> " <> txt
   <> maybe "" (\t -> "\n*[DeepL] " <> t <> "*") tl

tlMsg :: Text -> Text -> Text
tlMsg author txt =
  ":speech_balloon: ||" <> author <> ":|| `" <> txt <> "`"

isTl :: Text -> Bool
isTl = hasTlPrefix . T.stripStart . T.toLower where
  hasTlPrefix msg = any (`T.isPrefixOf` msg) tlPrefixes
  tlPrefixes      = ["[en", "tl:", "[tl", "en:", "eng:"]

data Msg = Msg { content :: Text, name :: Text, chId :: Text }

extract :: Value -> Either Text Msg
extract val = do
  content' <- pure val ?. "content" >>= unStr
  name' <- pure val ?. "authorName" >>= unStr
  chId' <- pure val ?. "authorChannelId" >>= unStr
  pure $ Msg content' name' chId'
