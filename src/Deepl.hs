{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Deepl (translate) where

-- Ado Bot modules
import Deepl.Internal (Tl (..), translateApi)
import Network        (fetchJson)
import Utils          ((<&>>=))

-------------------------------------------------------------------------------

translate :: MonadIO m => Text -> m (Either Text Text)
translate txt =
  fetchJson (translateApi txt)
    <&>>= \(Tl { srcLang, tlTxt }) -> case srcLang of
      "EN" -> Left "[DeepL] Source text was already English"
      _    -> Right tlTxt
