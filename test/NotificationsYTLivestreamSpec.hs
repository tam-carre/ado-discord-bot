{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module NotificationsYTLivestreamSpec (spec) where

-- Ado Bot modules
import Notifications.YTLivestream.Internal (VideoIdExtraction (..))

-- Downloaded libraries
import Test.Hspec (Spec, shouldBe, it)
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

spec :: Spec
spec =
  it "Should succeed parsing a valid payload" $ do
    let
      expectedResult = Right (VideoId "dummy")
      validPayload = "{ \"videoId\": \"dummy\", \"thumbnail\": { \"thumbnails\": [ { \"url\": \"dummy\", \"width\": 168, \"height\": 94 }, { \"url\": \"dummy\", \"width\": 196, \"height\": 110 }, { \"url\": \"dummy\", \"width\": 246, \"height\": 138 }, { \"url\": \"dummy\", \"width\": 336, \"height\": 188 } ] }, \"title\": { \"runs\": [ { \"text\": \"dummy\" } ], \"accessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } } }, \"descriptionSnippet\": { \"runs\": [ { \"text\": \"dummy\" } ] }, \"longBylineText\": { \"runs\": [ { \"text\": \"dummy\", \"navigationEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"url\": \"dummy\", \"webPageType\": \"dummy\", \"rootVe\": 3611, \"apiUrl\": \"dummy\" } }, \"browseEndpoint\": { \"browseId\": \"dummy\", \"canonicalBaseUrl\": \"dummy\" } } } ] }, \"viewCountText\": { \"runs\": [ { \"text\": \"dummy\" }, { \"text\": \"dummy\" } ] }, \"navigationEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"url\": \"dummy\", \"webPageType\": \"dummy\", \"rootVe\": 3832 } }, \"watchEndpoint\": { \"videoId\": \"dummy\", \"watchEndpointSupportedOnesieConfig\": { \"html5PlaybackOnesieConfig\": { \"commonConfig\": { \"url\": \"dummy\" } } } } }, \"ownerText\": { \"runs\": [ { \"text\": \"dummy\", \"navigationEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"url\": \"dummy\", \"webPageType\": \"dummy\", \"rootVe\": 3611, \"apiUrl\": \"dummy\" } }, \"browseEndpoint\": { \"browseId\": \"dummy\", \"canonicalBaseUrl\": \"dummy\" } } } ] }, \"shortBylineText\": { \"runs\": [ { \"text\": \"dummy\", \"navigationEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"url\": \"dummy\", \"webPageType\": \"dummy\", \"rootVe\": 3611, \"apiUrl\": \"dummy\" } }, \"browseEndpoint\": { \"browseId\": \"dummy\", \"canonicalBaseUrl\": \"dummy\" } } } ] }, \"trackingParams\": \"dummy\", \"showActionMenu\": false, \"shortViewCountText\": { \"runs\": [ { \"text\": \"dummy\" }, { \"text\": \"dummy\" } ] }, \"menu\": { \"menuRenderer\": { \"items\": [ { \"menuServiceItemRenderer\": { \"text\": { \"runs\": [ { \"text\": \"dummy\" } ] }, \"icon\": { \"iconType\": \"dummy\" }, \"serviceEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true } }, \"signalServiceEndpoint\": { \"signal\": \"dummy\", \"actions\": [ { \"clickTrackingParams\": \"dummy\", \"addToPlaylistCommand\": { \"openMiniplayer\": true, \"videoId\": \"dummy\", \"listType\": \"dummy\", \"onCreateListCommand\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true, \"apiUrl\": \"dummy\" } }, \"createPlaylistServiceEndpoint\": { \"videoIds\": [ { \"0\": \"dummy\", \"1\": \"dummy\", \"2\": \"dummy\", \"3\": \"dummy\", \"4\": \"dummy\", \"5\": \"dummy\", \"6\": \"dummy\", \"7\": \"dummy\", \"8\": \"dummy\", \"9\": \"dummy\", \"10\": \"dummy\" } ], \"params\": \"dummy\" } }, \"videoIds\": [ { \"0\": \"dummy\", \"1\": \"dummy\", \"2\": \"dummy\", \"3\": \"dummy\", \"4\": \"dummy\", \"5\": \"dummy\", \"6\": \"dummy\", \"7\": \"dummy\", \"8\": \"dummy\", \"9\": \"dummy\", \"10\": \"dummy\" } ] } } ] } }, \"trackingParams\": \"dummy\" } }, { \"menuServiceItemRenderer\": { \"text\": { \"runs\": [ { \"text\": \"dummy\" } ] }, \"icon\": { \"iconType\": \"dummy\" }, \"serviceEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true, \"apiUrl\": \"dummy\" } }, \"playlistEditEndpoint\": { \"playlistId\": \"dummy\", \"actions\": [ { \"addedVideoId\": \"dummy\", \"action\": \"dummy\" } ] } }, \"trackingParams\": \"dummy\" } }, { \"menuServiceItemRenderer\": { \"text\": { \"runs\": [ { \"text\": \"dummy\" } ] }, \"icon\": { \"iconType\": \"dummy\" }, \"serviceEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true, \"apiUrl\": \"dummy\" } }, \"addToPlaylistServiceEndpoint\": { \"videoId\": \"dummy\" } }, \"trackingParams\": \"dummy\" } } ], \"trackingParams\": \"dummy\", \"accessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } } } }, \"channelThumbnailSupportedRenderers\": { \"channelThumbnailWithLinkRenderer\": { \"thumbnail\": { \"thumbnails\": [ { \"url\": \"dummy\", \"width\": 68, \"height\": 68 } ] }, \"navigationEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"url\": \"dummy\", \"webPageType\": \"dummy\", \"rootVe\": 3611, \"apiUrl\": \"dummy\" } }, \"browseEndpoint\": { \"browseId\": \"dummy\", \"canonicalBaseUrl\": \"dummy\" } }, \"accessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } } } }, \"thumbnailOverlays\": [ { \"thumbnailOverlayTimeStatusRenderer\": { \"text\": { \"runs\": [ { \"text\": \"dummy\" } ], \"accessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } } }, \"style\": \"LIVE\", \"icon\": { \"iconType\": \"dummy\" } } }, { \"thumbnailOverlayToggleButtonRenderer\": { \"isToggled\": false, \"untoggledIcon\": { \"iconType\": \"dummy\" }, \"toggledIcon\": { \"iconType\": \"dummy\" }, \"untoggledTooltip\": \"dummy\", \"toggledTooltip\": \"dummy\", \"untoggledServiceEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true, \"apiUrl\": \"dummy\" } }, \"playlistEditEndpoint\": { \"playlistId\": \"dummy\", \"actions\": [ { \"addedVideoId\": \"dummy\", \"action\": \"dummy\" } ] } }, \"toggledServiceEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true, \"apiUrl\": \"dummy\" } }, \"playlistEditEndpoint\": { \"playlistId\": \"dummy\", \"actions\": [ { \"action\": \"dummy\", \"removedVideoId\": \"dummy\" } ] } }, \"untoggledAccessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } }, \"toggledAccessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } }, \"trackingParams\": \"dummy\" } }, { \"thumbnailOverlayToggleButtonRenderer\": { \"untoggledIcon\": { \"iconType\": \"dummy\" }, \"toggledIcon\": { \"iconType\": \"dummy\" }, \"untoggledTooltip\": \"dummy\", \"toggledTooltip\": \"dummy\", \"untoggledServiceEndpoint\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true } }, \"signalServiceEndpoint\": { \"signal\": \"dummy\", \"actions\": [ { \"clickTrackingParams\": \"dummy\", \"addToPlaylistCommand\": { \"openMiniplayer\": true, \"videoId\": \"dummy\", \"listType\": \"dummy\", \"onCreateListCommand\": { \"clickTrackingParams\": \"dummy\", \"commandMetadata\": { \"webCommandMetadata\": { \"sendPost\": true, \"apiUrl\": \"dummy\" } }, \"createPlaylistServiceEndpoint\": { \"videoIds\": [ { \"0\": \"dummy\", \"1\": \"dummy\", \"2\": \"dummy\", \"3\": \"dummy\", \"4\": \"dummy\", \"5\": \"dummy\", \"6\": \"dummy\", \"7\": \"dummy\", \"8\": \"dummy\", \"9\": \"dummy\", \"10\": \"dummy\" } ], \"params\": \"dummy\" } }, \"videoIds\": [ { \"0\": \"dummy\", \"1\": \"dummy\", \"2\": \"dummy\", \"3\": \"dummy\", \"4\": \"dummy\", \"5\": \"dummy\", \"6\": \"dummy\", \"7\": \"dummy\", \"8\": \"dummy\", \"9\": \"dummy\", \"10\": \"dummy\" } ] } } ] } }, \"untoggledAccessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } }, \"toggledAccessibility\": { \"accessibilityData\": { \"label\": \"dummy\" } }, \"trackingParams\": \"dummy\" } }, { \"thumbnailOverlayNowPlayingRenderer\": { \"text\": { \"runs\": [ { \"text\": \"dummy\" } ] } } } ] }"


    eitherDecode @VideoIdExtraction validPayload `shouldBe` expectedResult