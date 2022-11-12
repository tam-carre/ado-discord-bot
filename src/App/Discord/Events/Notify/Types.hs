module App.Discord.Events.Notify.Types (Notif (..)) where

import App.Discord.Guilds.Settings (GuildSettings)
import Data.Default                (Default (def))

----------------------------------------------------------------------------------------------------

data Notif
  = Notif
    { _settingsToCh   ∷ GuildSettings → Maybe Word64
    , _settingsToRole ∷ GuildSettings → Maybe Word64
    , _thumb          ∷ Maybe Text
    , _author         ∷ Text
    , _embedContent   ∷ Text
    , _embedUrl       ∷ Text
    , _msgTxt         ∷ Maybe Text
    }

instance Default Notif where
  def = Notif (const Nothing) (const Nothing) Nothing "" "" "" Nothing
