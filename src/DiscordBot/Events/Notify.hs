{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module DiscordBot.Events.Notify (notify, Notif (..)) where

-- Ado Bot modules
import DiscordBot.SendMessage (sendWithEmbed', embed)
import Utils                  (decorate)
import DiscordBot.Guilds.Settings
  ( SettingsDb
  , getAllSettings
  , GuildSettings (..)
  )

-- Downloaded libraries
import Data.Acid     (AcidState)
import Discord       (DiscordHandler)
import Discord.Types (CreateEmbed (..), CreateEmbedImage (..))
import qualified Data.Map  as Map
import qualified Data.Text as Text

-------------------------------------------------------------------------------

data Notif = Notif
  { settingsToCh   :: GuildSettings -> Maybe Word64
  , settingsToRole :: GuildSettings -> Maybe Word64
  , nThumb         :: Maybe Text
  , nAuthor        :: Text
  , embedContent   :: Text
  , embedUrl       :: Text
  , msgTxt         :: Maybe Text
  }

notify :: AcidState SettingsDb -> Notif -> DiscordHandler ()
notify db n = do
  allSettings     <- getAllSettings db
  let chAndRole gs = n.settingsToCh gs <&> (, n.settingsToRole gs)
      pendingMsgs  = mapMaybe chAndRole $ Map.elems allSettings
      avvie        = CreateEmbedImageUrl <$> n.nThumb
      embedTxt     = if Text.length n.embedContent < 2000
                       then n.embedContent
                       else Text.take 1999 n.embedContent <> "…"
      msgEmbed     = embed { createEmbedAuthorName  = n.nAuthor
                           , createEmbedAuthorIcon  = avvie
                           , createEmbedThumbnail   = avvie
                           , createEmbedDescription = embedTxt
                           , createEmbedUrl         = n.embedUrl
                           }
      msg role     = case n.msgTxt of
                       Nothing -> ""
                       Just txt -> ":loudspeaker: "
                                <> maybe "" (decorate "<@&" "> " . show) role
                                <> txt

  forM_ pendingMsgs $ \(ch, role) -> sendWithEmbed' ch (msg role) msgEmbed
