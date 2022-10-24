{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.NewCommunityPost (onNewCommunityPost) where

-- Ado Bot modules
import DiscordBot.SendMessage                      (sendWithEmbed', embed)
import Notifications.YouTubeCommunityPosts.Watcher (CommunityPost (..))
import Utils                                       (decorate)

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

onNewCommunityPost :: AcidState SettingsDb -> CommunityPost -> DiscordHandler ()
onNewCommunityPost settingsDb post = do
  allSettings     <- getAllSettings settingsDb
  let chAndRole gs = gs.communityPostCh <&> (, communityPostRole gs)
      pendingMsgs  = mapMaybe chAndRole $ Map.elems allSettings
      channelPfp   = Just . CreateEmbedImageUrl $ post.avatar
      postTxt      = if Text.length post.content < 2000
                       then post.content
                       else Text.take 1999 post.content <> "…"
      msgEmbed     = embed { createEmbedAuthorName  = post.author
                           , createEmbedAuthorIcon  = channelPfp
                           , createEmbedThumbnail   = channelPfp
                           , createEmbedDescription = postTxt
                           }
      msgTxt role  = ":loudspeaker: "
                  <> maybe "" (decorate "<@&" "> " . show) role
                  <> "Ado has just published a community post! \n"
                  <> "<https://youtube.com/post/" <> post.postId <> ">"

  forM_ pendingMsgs $ \(ch, role) -> sendWithEmbed' ch (msgTxt role) msgEmbed
