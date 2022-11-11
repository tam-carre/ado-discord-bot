{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}

module Notifications.SecretBase.Internal
  ( Lives (..)
  , SecretBaseLive (..)
  , Vids (..)
  , SecretBaseVid (..)
  ) where

-- Downloaded libraries
import Data.Aeson       (FromJSON (parseJSON), (.:), (.:?), withObject)
import Data.Aeson.Lens  (key, values, AsValue)
import Data.Aeson.Types (Parser)
import Data.Time        (parseTimeM, defaultTimeLocale, UTCTime)
import Control.Lens     ((^..))

-------------------------------------------------------------------------------

data SecretBaseLive = SecretBaseLive
  { _thumb   :: Text
  , _desc    :: Text
  , _title   :: Text
  , _url     :: Text
  , _started :: Bool
  } deriving (Show, Eq)

newtype Lives = Lives [SecretBaseLive] deriving (Show, Eq)

instance FromJSON Lives where
  parseJSON = vidListParser (Lives . filter _started)

instance FromJSON SecretBaseLive where
  parseJSON = withObject "single live object" $ \obj -> do
    let baseUrl = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/"
    url <- (baseUrl <>) <$> obj .: "content_code"
    SecretBaseLive <$> obj .: "thumbnail_url"
                   <*> pure ("Watch at <" <> url <> ">")
                   <*> obj .: "title"
                   <*> pure url
                   <*> ((.:?) @Text obj "live_started_at" <&> isJust)

data SecretBaseVid = SecretBaseVid
  { _thumb   :: Text
  , _desc    :: Text
  , _title   :: Text
  , _url     :: Text
  , _date    :: UTCTime
  } deriving (Show, Eq)

newtype Vids = Vids [SecretBaseVid] deriving (Show, Eq)

instance FromJSON Vids where
  parseJSON = vidListParser Vids

instance FromJSON SecretBaseVid where
  parseJSON = withObject "single live object" $ \obj -> do
    url <- (frameBaseUrl <>) <$> obj .: "content_code"
    dateStr <- obj .: "released_at"
    date <- parseTimeM @Parser @UTCTime True defaultTimeLocale "%Y-%-m-%-d %T" dateStr
    SecretBaseVid <$> obj .: "thumbnail_url"
                  <*> pure ("Watch at <" <> url <> ">")
                  <*> obj .: "title"
                  <*> pure url
                  <*> pure date

vidListParser :: (FromJSON a, AsValue p) => ([a] -> b) -> p -> Parser b
vidListParser constructor val = do
  let liveObjs = val^..key "data".key "video_pages".key "list".values
  lives <- traverse parseJSON liveObjs
  pure $ constructor lives

frameBaseUrl :: Text
frameBaseUrl = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/"
