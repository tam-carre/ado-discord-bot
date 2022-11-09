{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Notifications.SecretBase.Internal (Lives (..), SecretBaseLive (..)) where

-- Downloaded libraries
import Data.Aeson      (FromJSON (parseJSON), (.:), (.:?), withObject)
import Data.Aeson.Lens (key, values)
import Control.Lens    ((^..))

-------------------------------------------------------------------------------

data SecretBaseLive = SecretBaseLive
  { sblThumb   :: Text
  , sblDesc    :: Text
  , sblTitle   :: Text
  , sblUrl     :: Text
  , sblStarted :: Bool
  } deriving (Show, Eq)

newtype Lives = Lives [SecretBaseLive] deriving (Show, Eq)

instance FromJSON Lives where
  parseJSON val = do
    let liveObjs = val^..key "data".key "video_pages".key "list".values
    lives <- traverse parseJSON liveObjs
    pure . Lives . filter sblStarted $ lives

instance FromJSON SecretBaseLive where
  parseJSON = withObject "single live object" $ \obj -> do
    let baseUrl = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/"
    url <- (baseUrl <>) <$> obj .: "content_code"
    SecretBaseLive <$> obj .: "thumbnail_url"
                   <*> pure ("Watch at <" <> url <> ">")
                   <*> obj .: "title"
                   <*> pure url
                   <*> ((.:?) @Text obj "live_started_at" <&> isJust)
