{-# LANGUAGE FieldSelectors #-}

module App.BotConfig (BotConfig (..), botConfig) where

import Tokens qualified

----------------------------------------------------------------------------------------------------

data BotConfig
  = BotConfig
    { _botToken            ∷ Text
    , _deeplKey            ∷ Text
    , _ownerDebugGuildId   ∷ Word64
    , _ownerDebugChannelId ∷ Word64
    , _ownerUserId         ∷ Word64
    , _inviteUrl           ∷ Text
    }

botConfig ∷ BotConfig
botConfig = BotConfig
  { _botToken            = Tokens.discord
  , _ownerDebugGuildId   = 797780320405553223
  , _ownerDebugChannelId = 1031895573218983968
  , _ownerUserId         = 150696503428644864
  , _deeplKey            = Tokens.deepl
  , _inviteUrl           = invite
  }

invite ∷ Text
invite = "https://discordapp.com/oauth2/authorize?client_id=1031887395467046923&scope=bot&permissions=517543873600"
