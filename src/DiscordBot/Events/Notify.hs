{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

module DiscordBot.Events.Notify (notify, Notif (..)) where

-- Ado Bot modules
import Lenses
import App                            (App, Env (..))
import Utils                          (decorate)
import DiscordBot.Events.Notify.Types (Notif (..))
import DiscordBot.SendMessage         (sendWithEmbed', embed)
import DiscordBot.Guilds.Settings     (getAllSettings)

-- Downloaded libraries
import Discord.Types (CreateEmbedImage (..))
import qualified Data.Map  as Map
import qualified Data.Text as T

-------------------------------------------------------------------------------

notify :: Notif -> App ()
notify n = do
  allSettings     <- getAllSettings =<< asks settingsDb
  let chAndRole gs = (n^.settingsToCh) gs <&> (, (n^.settingsToRole) gs)
      pendingMsgs  = mapMaybe chAndRole $ Map.elems allSettings
      avvie        = CreateEmbedImageUrl <$> n^.nThumb
      embedTxt     = if n^.embedContent.to T.length < 2000
                       then n^.embedContent
                       else n^.embedContent.to (T.take 1999) <> "…"
      msgEmbed     = embed & authorName  .~ n^.nAuthor
                           & authorIcon  .~ avvie
                           & thumbnail   .~ avvie
                           & description .~ embedTxt
                           & url         .~ n^.embedUrl
      msg role     = case n^.msgTxt of
                       Nothing -> ""
                       Just txt -> ":loudspeaker: "
                                <> maybe "" (decorate "<@&" "> " . show) role
                                <> txt

  forM_ pendingMsgs $ \(ch, role) -> sendWithEmbed' ch (msg role) msgEmbed
