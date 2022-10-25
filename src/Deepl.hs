{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Deepl (translate) where

-- Ado Bot modules
import BotConfig (botConfig, BotConfig (..))
import Network   (fetchJson)
import Utils     ((<&>>=))
import Json      ((?.), (?!!), unStr)

-- Downloaded libraries
import Data.Aeson          (Value (..))
import Network.HTTP.Simple (setRequestHeader, setRequestBodyURLEncoded, Request)

-------------------------------------------------------------------------------

translate :: MonadIO m => Text -> m (Either Text Text)
translate txt =
  fetchJson "DeepL" parseTranslations (req txt)
    <&>>= \(sourceLang, tl) -> case sourceLang of
      "EN" -> Left "[DeepL] Source text was already English"
      _    -> Right tl

req :: Text -> Request
req txt = "POST https://api-free.deepl.com/v2/translate"
  & setRequestHeader
      "Authorization" [ "DeepL-Auth-Key " <> encodeUtf8 botConfig.deeplKey ]
  & setRequestBodyURLEncoded
      [ ("target_lang", "EN")
      , ("text", encodeUtf8 txt)
      ]

parseTranslations :: Value -> Either Text (Text, Text)
parseTranslations json = do
  tl         <- pure json ?. "translations" ?!! 0
  sourceLang <- pure tl ?. "detected_source_language" >>= unStr
  translated <- pure tl ?. "text" >>= unStr
  pure (sourceLang, translated)
