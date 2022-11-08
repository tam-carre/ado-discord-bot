{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Deepl (translate) where

-- Ado Bot modules
import Deepl.Internal (Tl (..))
import BotConfig      (botConfig, BotConfig (..))
import Network        (fetchJson')
import Utils          ((<&>>=))

-- Downloaded libraries
import Network.HTTP.Simple (setRequestHeader, setRequestBodyURLEncoded, Request)

-------------------------------------------------------------------------------

translate :: MonadIO m => Text -> m (Either Text Text)
translate txt =
  fetchJson' req
    <&>>= \(Tl { srcLang, tlTxt }) -> case srcLang of
      "EN" -> Left "[DeepL] Source text was already English"
      _    -> Right tlTxt

  where
  req :: Request
  req = "POST https://api-free.deepl.com/v2/translate"
    & setRequestHeader
        "Authorization" [ "DeepL-Auth-Key " <> encodeUtf8 botConfig.deeplKey ]
    & setRequestBodyURLEncoded
        [ ("target_lang", "EN")
        , ("text", encodeUtf8 txt)
        ]
