{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module DiscordBot.Events.NewCommunityPost (onNewCommunityPost) where

-- Ado Bot modules
import Lenses
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
    { _settingsToCh   = _communityPostCh
    , _settingsToRole = _communityPostRole
    , _nThumb         = Just $ post^.avatar
    , _nAuthor        = post^.author
    , _embedContent   = post^.content
    , _embedUrl       = "https://youtube.com/post/" <> (post^.postId)
    , _msgTxt         = Just $ "Ado has just published a community post! \n"
                     <> "<https://youtube.com/post/" <> (post^.postId) <> ">"
    }

  post^.content & translate >>= \case
    Left err -> echo err
    Right tl -> notify db Notif
      { _settingsToCh   = _communityPostCh
      , _settingsToRole = const Nothing
      , _nThumb         = Nothing
      , _nAuthor        = ""
      , _embedContent   = "*[DeepL]* " <> tl
      , _embedUrl       = "https://youtube.com/post/" <> (post^.postId)
      , _msgTxt         = Nothing
      }
