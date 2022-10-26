{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module DiscordBot.Events.NewCommunityPost (onNewCommunityPost) where

-- Ado Bot modules
import Deepl                          (translate)
import DiscordBot.Events.Notify       (notify, Notif (..))
import DiscordBot.Guilds.Settings     (SettingsDb, GuildSettings (..))
import Notifications.YTCommunityPosts (CommunityPost (..))

-- Downloaded libraries
import Discord   (DiscordHandler)
import Data.Acid (AcidState)

-------------------------------------------------------------------------------

onNewCommunityPost :: AcidState SettingsDb -> CommunityPost -> DiscordHandler ()
onNewCommunityPost db post = do
  notify db Notif
    { settingsToCh   = communityPostCh
    , settingsToRole = communityPostRole
    , nThumb         = Just post.avatar
    , nAuthor        = post.author
    , embedContent   = post.content
    , embedUrl       = "https://youtube.com/post/" <> post.postId
    , msgTxt         = Just $ "Ado has just published a community post! \n"
                    <> "<https://youtube.com/post/" <> post.postId <> ">"
    }

  translate post.content >>= \case
    Left err -> echo err
    Right tl -> notify db Notif
      { settingsToCh   = communityPostCh
      , settingsToRole = const Nothing
      , nThumb         = Nothing
      , nAuthor        = ""
      , embedContent   = "*[DeepL]* " <> tl
      , embedUrl       = "https://youtube.com/post/" <> post.postId
      , msgTxt         = Nothing
      }
