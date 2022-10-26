{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Notifications.SecretBase
  ( SecretBaseLive (..)
  , getNextNewSecretBase
  ) where

-- Ado Bot modules
import Network             (fetchJson)
import Utils               ((>>>=))
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
latestSecretBase db = fetchJson "Secret Base" jsonToLives url >>>= \case
  [] -> err "No ongoing live"
  lives -> do
    notifHistory <- getNotifHistory db
    let new = filter ((`notElem` notifHistory.secretBase) . sblUrl) lives
    case listToMaybe new of
      Nothing -> err "Ongoing live found but already notified"
      Just live -> do
        changeNotifHistory db $ \hist -> hist
          { secretBase = live.sblUrl : take 50 hist.secretBase }

        pure $ Right live
  where
  url = "https://nfc-api.ado-dokidokihimitsukichi-daigakuimo.com/fc/fanclub_sites/95/live_pages?page=1&live_type=1&per_page=1"
  err = pure . Left . ("[Secret Base] " <>)

jsonToLives :: Value -> Either Text [SecretBaseLive]
jsonToLives json = do
  liveObjs <- pure json ?. "data" ?. "video_pages" ?. "list" >>= unArr
  let lives = traverse singleLiveJsonToHaskell (Vector.toList liveObjs)
  filter sblStarted <$> lives

singleLiveJsonToHaskell :: Value -> Either Text SecretBaseLive
singleLiveJsonToHaskell json = do
  let prop name = pure json ?. name >>= unStr
      isStarted = isRight $ prop "live_started_at"

  thumb <- prop "thumbnail_url"
  title <- prop "title"
  code  <- prop "content_code"

  let url = "https://ado-dokidokihimitsukichi-daigakuimo.com/video/" <> code

  pure SecretBaseLive { sblThumb   = thumb
                      , sblDesc    = "Watch at <" <> url <> ">"
                      , sblTitle   = title
                      , sblUrl     = url
                      , sblStarted = isStarted
                      }
