{-# LANGUAGE OverloadedStrings #-}

module Deepl.Internal (Tl (..), translateApi) where

-- Downloaded libraries
import Data.Aeson          (FromJSON (..), (.:), withObject)
import BotConfig           (botConfig, BotConfig (..))
import Data.Aeson.Lens     (key, nth)
import Control.Lens        ((^?))
import Network.HTTP.Simple (setRequestHeader, setRequestBodyURLEncoded, Request)

-------------------------------------------------------------------------------

data Tl = Tl { srcLang :: Text, tlTxt :: Text } deriving (Show, Eq)

instance FromJSON Tl where
  parseJSON val = case val ^? key "translations" . nth 0 of
    Nothing -> fail "Couldn't find resp.translations[0]"
    Just tlObj -> tlObj & withObject "tlObj"
      (\obj -> Tl <$> obj .: "detected_source_language"
                  <*> obj .: "text"
      )

translateApi :: Text -> Request
translateApi txt = "POST https://api-free.deepl.com/v2/translate"
  & setRequestHeader
      "Authorization" [ "DeepL-Auth-Key " <> encodeUtf8 (_deeplKey botConfig) ]
  & setRequestBodyURLEncoded
      [ ("target_lang", "EN")
      , ("text", encodeUtf8 txt)
      ]
