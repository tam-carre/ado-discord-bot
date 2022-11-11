{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module DiscordBot.Events.NewYTChatMsg (onNewYTChatMsg) where

-- Ado Bot modules
import Lenses
import App                        (App, Env (..))
import Deepl                      (translate)
import DiscordBot.SendMessage     (send')
import DiscordBot.Guilds.Settings (getAllSettings)

-- Downloaded libraries
import Data.Aeson (withObject, (.:), eitherDecode, FromJSON (..))
import qualified Data.Map                as Map
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Data.Text.Lazy          as L
import qualified Data.Text               as T

-------------------------------------------------------------------------------

data Msg = Msg { _content :: Text, _name :: Text, _chId :: Text }
makeFieldsNoPrefix ''Msg

instance FromJSON Msg where
  parseJSON = withObject "masterchat msg" $ \msg ->
    Msg <$> msg .: "content"
        <*> msg .: "authorName"
        <*> msg .: "authorChannelId"

onNewYTChatMsg :: L.Text -> App (Either Void ())
onNewYTChatMsg line = do
  allSettings <- getAllSettings =<< asks settingsDb
  let channelsAwaitingMsg = mapMaybe (view relayCh) $ Map.elems allSettings
  case eitherDecode @Msg (Lazy.Encoding.encodeUtf8 line) of
    Left e -> err $ "error parsing msg " <> toText e
    Right msg
      | isAdo msg -> do
        echo "Found message by Ado, relaying"
        tl <- rightToMaybe <$> translate (msg^.content)
        forM_ channelsAwaitingMsg . send' $ adoMsg msg tl
        done

      | isTl (msg^.content) -> do
        echo "Found live translation, relaying"
        forM_ channelsAwaitingMsg . send' $ tlMsg (msg^.name) (msg^.content)
        done

      | otherwise ->
        done
  where
  err x = echo x >> done
  done = pure $ Right ()
  isAdo msg = msg^.chId == "UCln9P4Qm3-EAY4aiEPmRwEA"
  adoMsg msg tl =
    "<:AdoHappy:885833189086593024> **" <> (msg^.name) <> "**: " <> (msg^.content)
     <> maybe "" (\t -> "\n*[DeepL] " <> t <> "*") tl
  tlMsg author' txt =
    ":speech_balloon: ||" <> author' <> ":|| `" <> txt <> "`"
  isTl = hasTlPrefix . T.stripStart . T.toLower where
    hasTlPrefix msg = any (`T.isPrefixOf` msg) tlPrefixes
    tlPrefixes      = ["[en", "tl:", "[tl", "en:", "eng:"]

