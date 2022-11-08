{-# LANGUAGE OverloadedStrings #-}

module Notifications.YTLivestream.Internal (VideoIdExtraction (..)) where

-- Downloaded libraries
import Data.Aeson      (FromJSON (..))
import Data.Aeson.Lens (key, _String, nth)
import Control.Lens    ((^.))

-------------------------------------------------------------------------------

data VideoIdExtraction
  = VideoId Text
  | NotLive
  deriving (Show, Eq)

instance FromJSON VideoIdExtraction where
  parseJSON ytData =
    let vidId = ytData^.key "videoId"._String
        style = ytData^.key "thumbnailOverlays".nth 0
                       .key "thumbnailOverlayTimeStatusRenderer"
                       .key "style"._String
    in
    pure $ case style of
    "LIVE" -> VideoId vidId
    _      -> NotLive
