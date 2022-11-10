{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Notifications.YTLivestream (VideoId, getNextNewLivestream) where

-- Ado Bot modules
import Lenses
import App                                 (App)
import Notifications.YTLivestream.Internal (VideoIdExtraction (..))
import Utils                               (betweenSubstrs)
import Notifications.Utils                 (returnWhenFound)
import Notifications.History               (getNotifHistory, changeNotifHistory)

-- Downloaded libraries
import Data.Aeson (eitherDecode)
import Network.HTTP.Simple
  ( parseRequest
  , getResponseStatusCode
  , getResponseBody
  , httpBS
  , setRequestHeader
  )
import qualified Data.Text.Lazy.Encoding    as Lazy.Encoding
import qualified Data.ByteString.Lazy.Char8 as L8

-------------------------------------------------------------------------------

type VideoId = Text

-- | This function only returns once Ado goes live on YouTube
getNextNewLivestream :: App VideoId
getNextNewLivestream = returnWhenFound newLivestream "New livestream"

-- | Returns the video ID of Ado's just-started livestream, or a Left explaining
-- what problem it encountered.
newLivestream :: App (Either Text VideoId)
newLivestream = do
  request <- liftIO $ parseRequest "GET https://www.youtube.com/c/Ado1024"
  response <- httpBS . setRequestHeader "Accept-Language" ["en"] $ request

  let status  = getResponseStatusCode response
      jsonStr = getPayload . decodeUtf8 $ getResponseBody response

  case (status, jsonStr) of
    (200, Just payload) ->
      case eitherDecode @VideoIdExtraction payload of
        Right (VideoId vidId) -> do
          notifHistory <- getNotifHistory
          if vidId `notElem` (notifHistory^.ytStream) then do
            changeNotifHistory . over ytStream $ \h -> vidId : take 50 h

            pure $ Right vidId

          else err "Found livestream already notified"

        Right NotLive -> err "Found stream but not live yet"
        Left e        -> err $ "Failed to extract: " <> show e

    (200, Nothing) -> err "No ongoing live"
    _              -> err "Non-200 status code"

  where
  err  = pure . Left . ("[Livestream] " <>)

-- | Takes the page source, returns only the embedded JSON of the first
-- livestream as ByteString
getPayload :: Text -> Maybe L8.ByteString
getPayload = fmap (Lazy.Encoding.encodeUtf8 . fromStrict)
  . betweenSubstrs "[{\"videoRenderer\":" "}]}}],\"trackingParams\""
