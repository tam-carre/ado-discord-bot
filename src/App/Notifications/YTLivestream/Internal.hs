module App.Notifications.YTLivestream.Internal (VideoIdExtraction (..), getYtChannelPayload) where

import App.Utils                  (betweenSubstrs)
import Control.Lens               ((^.))
import Data.Aeson                 (FromJSON (..))
import Data.Aeson.Lens            (_String, key, nth)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text.Lazy.Encoding    qualified as Lazy.Encoding
import Network.HTTP.Simple        (getResponseBody, getResponseStatusCode, httpBS, parseRequest,
                                   setRequestHeader)

----------------------------------------------------------------------------------------------------

data VideoIdExtraction
  = VideoId Text
  | NotLive
  deriving (Eq, Show)

instance FromJSON VideoIdExtraction where
  parseJSON ytData =
    let vidId = ytData^.key "videoId"._String
        style = ytData^.key "thumbnailOverlays".nth 0
                       .key "thumbnailOverlayTimeStatusRenderer"
                       .key "style"._String
    in
    pure $ case style of
    "LIVE" → VideoId vidId
    _      → NotLive

getYtChannelPayload ∷ MonadIO m ⇒ m (Int, Maybe L8.ByteString)
getYtChannelPayload = do
  request ← liftIO . parseRequest $ "GET https://www.youtube.com/c/Ado1024"
  response ← httpBS . setRequestHeader "Accept-Language" ["en"] $ request

  let status  = getResponseStatusCode response
      jsonStr = getPayload . decodeUtf8 $ getResponseBody response

  pure (status, jsonStr)
  where
  -- | Takes the page source, returns only the embedded JSON of the first
  -- livestream as ByteString
  getPayload = fmap (Lazy.Encoding.encodeUtf8 . fromStrict)
    . betweenSubstrs "[{\"videoRenderer\":" "}]}}],\"trackingParams\""
