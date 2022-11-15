{-# LANGUAGE DuplicateRecordFields, FieldSelectors, TypeApplications #-}

module App.Notifications.SecretBase.Internal
  ( Lives (..)
  , SecretBaseLive (..)
  , SecretBaseVid (..)
  , Vids (..)
  , secretBaseStreamsApi
  , secretBaseVidsApi
  ) where

import Control.Lens        ((^..))
import Data.Aeson          (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Lens     (AsValue, key, values)
import Data.Aeson.Types    (Parser)
import Data.Time           (UTCTime, defaultTimeLocale, parseTimeM)
import Network.HTTP.Client (Request)

----------------------------------------------------------------------------------------------------

data SecretBaseLive
  = SecretBaseLive
    { _thumb   ∷ Text
    , _desc    ∷ Text
    , _title   ∷ Text
    , _url     ∷ Text
    , _started ∷ Bool
    }
  deriving (Eq, Show)

newtype Lives
  = Lives [SecretBaseLive]
  deriving (Eq, Show)

instance FromJSON Lives where
  parseJSON = vidListParser (Lives . filter _started)

instance FromJSON SecretBaseLive where
  parseJSON = withObject "single live object" $ \obj → do
    url ← (frameBaseUrl <>) <$> obj .: "content_code"
    SecretBaseLive <$> obj .: "thumbnail_url"
                   <*> pure ("Watch at <" ⊕ url ⊕ ">")
                   <*> obj .: "title"
                   <*> pure url
                   <*> ((.:?) @Text obj "live_started_at" <&> isJust)

data SecretBaseVid
  = SecretBaseVid
    { _thumb ∷ Text
    , _desc  ∷ Text
    , _title ∷ Text
    , _url   ∷ Text
    , _date  ∷ UTCTime
    }
  deriving (Eq, Show)

newtype Vids
  = Vids [SecretBaseVid]
  deriving (Eq, Show)

instance FromJSON Vids where
  parseJSON = vidListParser Vids

instance FromJSON SecretBaseVid where
  parseJSON = withObject "single live object" $ \obj → do
    url ← (frameBaseUrl <>) <$> obj .: "content_code"
    dateStr ← obj .: "released_at"
    date ← parseTimeM @Parser @UTCTime True defaultTimeLocale "%Y-%-m-%-d %T" dateStr
    SecretBaseVid <$> obj .: "thumbnail_url"
                  <*> pure ("Watch at <" ⊕ url ⊕ ">")
                  <*> obj .: "title"
                  <*> pure url
                  <*> pure date

vidListParser ∷ (FromJSON a, AsValue p) ⇒ ([a] → b) → p → Parser b
vidListParser constructor val = do
  let liveObjs = val^..key "data".key "video_pages".key "list".values
  lives ← traverse parseJSON liveObjs
  pure $ constructor lives

frameBaseUrl ∷ Text
frameBaseUrl = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/"

secretBaseStreamsApi ∷ Request
secretBaseStreamsApi =
  "https://nfc-api.ado-dokidokihimitsukichi-daigakuimo.com/fc/fanclub_sites/95/live_pages?page=1&live_type=1&per_page=1"

secretBaseVidsApi ∷ Request
secretBaseVidsApi =
  "https://nfc-api.ado-dokidokihimitsukichi-daigakuimo.com/fc/fanclub_sites/95/video_pages?vod_type=0&tag=%E5%8B%95%E7%94%BB&sort=-released_at&page=1&per_page=12"
