module Config.Type (BotConfig (..)) where

-- Downloaded libraries
import Data.Text (Text)

-- Base
import Data.Word (Word64)

-------------------------------------------------------------------------------

data BotConfig = BotConfig
  { botToken            :: Text
  , deeplKey            :: Text
  , ownerDebugChannelId :: Word64
  , ownerUserId         :: Word64
  , inviteUrl           :: Text
  }
