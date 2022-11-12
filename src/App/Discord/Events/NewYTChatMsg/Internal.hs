{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TemplateHaskell, TypeApplications #-}

module App.Discord.Events.NewYTChatMsg.Internal (Msg (..), adoMsg, isAdo, isTl, tlMsg) where

import App.Utils    (decorate)
import Control.Lens (makeFieldsNoPrefix, (^.))
import Data.Aeson   (FromJSON (..), withObject, (.:))
import Data.Text    qualified as T

----------------------------------------------------------------------------------------------------

data Msg
  = Msg
    { _content ∷ Text
    , _name    ∷ Text
    , _chId    ∷ Text
    }

makeFieldsNoPrefix ''Msg

instance FromJSON Msg where
  parseJSON = withObject "masterchat msg" $ \msg →
    Msg <$> msg .: "content"
        <*> msg .: "authorName"
        <*> msg .: "authorChannelId"

isAdo ∷ Msg → Bool
isAdo msg = msg^.chId ≡ "UCln9P4Qm3-EAY4aiEPmRwEA"

adoMsg ∷ Msg → Maybe Text → Text
adoMsg msg tl =
  "<:AdoHappy:885833189086593024> **" ⊕ msg^.name ⊕ "**: " ⊕ msg^.content
   ⊕ maybe "" (decorate "\n*[DeepL] " "*") tl

tlMsg ∷ Text → Text → Text
tlMsg author' txt = ":speech_balloon: ||" ⊕ author' ⊕ ":|| `" ⊕ txt ⊕ "`"

isTl ∷ Text → Bool
isTl = hasTlPrefix . T.stripStart . T.toLower where
  hasTlPrefix msg = any (`T.isPrefixOf` msg) ["[en", "tl:", "[tl", "en:", "eng:"]
