{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module DiscordBot.Events.NewCommunityPost (onNewCommunityPost) where

-- Ado Bot modules
import DiscordBot.Events.Notify       (notify, Notif (..))
import DiscordBot.Guilds.Settings     (SettingsDb, GuildSettings (..))
import Notifications.YTCommunityPosts (CommunityPost (..))

-- Downloaded libraries
import Discord   (DiscordHandler)
import Data.Acid (AcidState)

-------------------------------------------------------------------------------

onNewCommunityPost :: AcidState SettingsDb -> CommunityPost -> DiscordHandler ()
onNewCommunityPost db post = notify db Notif
  { settingsToCh   = communityPostCh
  , settingsToRole = communityPostRole
  , ncThumb        = Just post.avatar
  , ncAuthor       = post.author
  , embedContent   = post.content
  , embedUrl       = "https://youtube.com/post/" <> post.postId
  , msgTxt         = "Ado has just published a community post! \n"
                  <> "<https://youtube.com/post/" <> post.postId <> ">"
  }
