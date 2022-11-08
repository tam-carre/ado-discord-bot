{-# LANGUAGE OverloadedStrings #-}

module Deepl.Internal (Tl (..)) where

-- Downloaded libraries
import Data.Aeson      (FromJSON (..), (.:), withObject)
import Data.Aeson.Lens (key, nth)
import Control.Lens    ((^?))

-------------------------------------------------------------------------------

data Tl = Tl { srcLang :: Text, tlTxt :: Text } deriving (Show, Eq)

instance FromJSON Tl where
  parseJSON val = case val ^? key "translations" . nth 0 of
    Nothing -> fail "Couldn't find resp.translations[0]"
    Just tlObj -> tlObj & withObject "tlObj"
      (\obj -> Tl <$> obj .: "detected_source_language"
                  <*> obj .: "text"
      )
