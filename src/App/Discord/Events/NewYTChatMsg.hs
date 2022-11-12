{-# LANGUAGE FlexibleInstances, TypeApplications #-}

module App.Discord.Events.NewYTChatMsg (onNewYTChatMsg) where

import App                                      (App, Env (..))
import App.Deepl                                (translate)
import App.Discord.Events.NewYTChatMsg.Internal (Msg (..), adoMsg, isAdo, isTl, tlMsg)
import App.Discord.Guilds.Settings              (getAllSettings)
import App.Discord.SendMessage                  (send')
import App.Lenses                               (content, name, relayCh, (^.))
import Data.Aeson                               (eitherDecode)
import Data.Map                                 qualified as Map
import Data.Text.Lazy                           qualified as L
import Data.Text.Lazy.Encoding                  qualified as Lazy.Encoding

----------------------------------------------------------------------------------------------------

onNewYTChatMsg ∷ L.Text → App (Either Void ())
onNewYTChatMsg line = do
  allSettings ← getAllSettings =≪ asks settingsDb
  let channelsAwaitingMsg = mapMaybe (^.relayCh) $ Map.elems allSettings
  case eitherDecode @Msg $ Lazy.Encoding.encodeUtf8 line of
    Left e → err $ "error parsing msg " ⊕ toText e
    Right msg
      | isAdo msg → do
        echo "Found message by Ado, relaying"
        tl ← rightToMaybe <$> translate (msg^.content)
        forM_ channelsAwaitingMsg . send' $ adoMsg msg tl
        done

      | isTl (msg^.content) → do
        echo "Found live translation, relaying"
        forM_ channelsAwaitingMsg . send' $ tlMsg (msg^.name) (msg^.content)
        done

      | otherwise →
        done
  where
  err x = echo x ≫ done
  done = pure $ Right ()
