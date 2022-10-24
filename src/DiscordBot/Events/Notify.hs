{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , ncThumb        :: Maybe Text
  , ncAuthor       :: Text
  , embedContent   :: Text
  , embedUrl       :: Text
  , msgTxt         :: Text
  }

notify :: AcidState SettingsDb -> Notif -> DiscordHandler ()
notify db nc = do
  allSettings     <- getAllSettings db
  let chAndRole gs = nc.settingsToCh gs <&> (, nc.settingsToRole gs)
      pendingMsgs  = mapMaybe chAndRole $ Map.elems allSettings
      avvie        = CreateEmbedImageUrl <$> nc.ncThumb
      embedTxt     = if Text.length nc.embedContent < 2000
                       then nc.embedContent
                       else Text.take 1999 nc.embedContent <> "…"
      msgEmbed     = embed { createEmbedAuthorName  = nc.ncAuthor
                           , createEmbedAuthorIcon  = avvie
                           , createEmbedThumbnail   = avvie
                           , createEmbedDescription = embedTxt
                           , createEmbedUrl         = nc.embedUrl
                           }
      msg role     = ":loudspeaker: "
                  <> maybe "" (decorate "<@&" "> " . show) role
                  <> nc.msgTxt

  forM_ pendingMsgs $ \(ch, role) -> sendWithEmbed' ch (msg role) msgEmbed
