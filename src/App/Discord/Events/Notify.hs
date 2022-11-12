{-# LANGUAGE TupleSections #-}

module App.Discord.Events.Notify (Notif (..), notify) where

import App                             (App, Env (..))
import App.Discord.Events.Notify.Types (Notif (..))
import App.Discord.Guilds.Settings     (getAllSettings)
import App.Discord.SendMessage         (embed, sendWithEmbed')
import App.Lenses                      (author, authorIcon, authorName, description, embedContent,
                                        embedUrl, msgTxt, settingsToCh, settingsToRole, thumb,
                                        thumbnail, url, (.~), (^.))
import App.Utils                       (decorate, trunc)
import Data.Map                        qualified as Map
import Discord.Types                   (CreateEmbedImage (..))

----------------------------------------------------------------------------------------------------

notify ∷ Notif → App ()
notify n = do
  allSettings ← getAllSettings =≪ asks settingsDb
  let chAndRole gs = (n^.settingsToCh) gs <&> (, (n^.settingsToRole) gs)
      pendingMsgs  = mapMaybe chAndRole $ Map.elems allSettings
      avvie        = CreateEmbedImageUrl <$> n^.thumb
      embedTxt     = n^.embedContent & trunc 1999
      msgEmbed     = embed & authorName  .~ n^.author
                           & authorIcon  .~ avvie
                           & thumbnail   .~ avvie
                           & description .~ embedTxt
                           & url         .~ n^.embedUrl
      msg role     = case n^.msgTxt of
                       Nothing  → ""
                       Just txt → ":loudspeaker: "
                                ⊕ maybe "" (decorate "<@&" "> " . show) role
                                ⊕ txt

  forM_ pendingMsgs $ \(ch, role) → sendWithEmbed' ch (msg role) msgEmbed
