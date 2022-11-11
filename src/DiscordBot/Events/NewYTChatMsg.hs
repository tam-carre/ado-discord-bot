{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}

module DiscordBot.Events.NewYTChatMsg (onNewYTChatMsg) where

-- Ado Bot modules
import Lenses
import App                        (App, Env (..))
import Deepl                      (translate)
import DiscordBot.SendMessage     (send')
import DiscordBot.Guilds.Settings (getAllSettings)
import DiscordBot.Events.NewYTChatMsg.Internal
  ( Msg (..)
  , isAdo
  , adoMsg
  , tlMsg
  , isTl
  )

-- Downloaded libraries
import Data.Aeson (eitherDecode)
import qualified Data.Map                as Map
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Data.Text.Lazy          as L

-------------------------------------------------------------------------------

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
