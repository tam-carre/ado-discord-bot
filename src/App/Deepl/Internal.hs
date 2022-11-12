module App.Deepl.Internal (Tl (..), translateApi) where

import App.BotConfig       (BotConfig (..), botConfig)
import Control.Lens        ((^?))
import Data.Aeson          (FromJSON (..), withObject, (.:))
import Data.Aeson.Lens     (key, nth)
import Network.HTTP.Simple (Request, setRequestBodyURLEncoded, setRequestHeader)

----------------------------------------------------------------------------------------------------

data Tl
  = Tl
    { srcLang ∷ Text
    , tlTxt   ∷ Text
    }
  deriving (Eq, Show)

instance FromJSON Tl where
  parseJSON val = case val ^? key "translations" . nth 0 of
    Nothing    → fail "Couldn't find resp.translations[0]"
    Just tlObj → tlObj & withObject "tlObj"
      (\obj → Tl <$> obj .: "detected_source_language"
                 <*> obj .: "text"
      )

translateApi ∷ Text → Request
translateApi txt = "POST https://api-free.deepl.com/v2/translate"
  & setRequestHeader
    "Authorization" [ "DeepL-Auth-Key " ⊕ encodeUtf8 (_deeplKey botConfig) ]
  & setRequestBodyURLEncoded
    [ ("target_lang", "EN")
    , ("text", encodeUtf8 txt)
    ]
