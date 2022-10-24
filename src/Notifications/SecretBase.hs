{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Notifications.SecretBase
  ( SecretBaseLive (..), getNextNewSecretBase
  ) where

-- Ado Bot modules
import Json                ((?.), unArr, unStr)
import Notifications.Utils (returnWhenFound)
import Notifications.History
  ( NotifHistoryDb (..)
  , getNotifHistory
  , changeNotifHistory
  )

-- Downloaded libraries
import Data.Acid  (AcidState)
import Data.Aeson (Value (..))
import Network.HTTP.Simple
  ( getResponseStatusCode
  , getResponseBody
  , httpJSONEither
  , JSONException
  )

import qualified Data.Vector as Vector

-------------------------------------------------------------------------------

data SecretBaseLive = SecretBaseLive
  { sblThumb   :: Text
  , sblDesc    :: Text
  , sblTitle   :: Text
  , sblUrl     :: Text
  , sblStarted :: Bool
  }

-- | This function only returns once Ado goes live on Secret Base
getNextNewSecretBase :: MonadIO m => AcidState NotifHistoryDb -> m SecretBaseLive
getNextNewSecretBase = returnWhenFound latestSecretBase "New Secret Base"

-- | Returns a freshly started Secret Base stream by Ado, or a Left explaining
-- what problem it encountered.
latestSecretBase :: MonadIO m => AcidState NotifHistoryDb -> m (Either Text SecretBaseLive)
latestSecretBase db = do
  response <- httpJSONEither url

  let json :: Either JSONException Value
      json   = getResponseBody response
      status = getResponseStatusCode response

  case (status, json) of
    (200, Right value) -> case jsonToHaskell value of
      Nothing -> err "Failed to extract data out of JSON"
      Just [] -> err "No ongoing live"
      Just lives -> do
        notifHistory <- getNotifHistory db
        let new = filter ((`notElem` notifHistory.secretBase) . sblUrl) lives
        case listToMaybe new of
          Nothing -> err "Ongoing live found but already notified"
          Just live -> do
            changeNotifHistory db $ \hist -> hist
              { secretBase = live.sblUrl : take 50 hist.secretBase }

            pure $ Right live

    (200, _) -> err "Failed to decode JSON"
    _ -> err "Non-200 status code"

  where
  url = "https://nfc-api.ado-dokidokihimitsukichi-daigakuimo.com/fc/fanclub_sites/95/live_pages?page=1&live_type=1&per_page=1"
  err = pure . Left . ("[Secret Base] " <>)

jsonToHaskell :: Value -> Maybe [SecretBaseLive]
jsonToHaskell json = do
  liveObjs <- Just json ?. "data" ?. "video_pages" ?. "list" >>= unArr
  let lives = traverse singleLiveJsonToHaskell (Vector.toList liveObjs)
  filter sblStarted <$> lives

singleLiveJsonToHaskell :: Value -> Maybe SecretBaseLive
singleLiveJsonToHaskell json = do
  let prop name = Just json ?. name >>= unStr
      isStarted = isJust $ prop "live_started_at"

  thumb <- prop "thumbnail_url"
  title <- prop "title"
  code  <- prop "content_code"

  let url = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/" <> code

  Just SecretBaseLive { sblThumb   = thumb
                      , sblDesc    = "Watch at <" <> url <> ">"
                      , sblTitle   = title
                      , sblUrl     = url
                      , sblStarted = isStarted
                      }
