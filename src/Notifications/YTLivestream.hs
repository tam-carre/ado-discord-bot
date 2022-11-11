{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}

module Notifications.YTLivestream (VideoId, getNextNewLivestream) where

-- Ado Bot modules
import Lenses
import App                   (App)
import Notifications.Utils   (returnWhenFound)
import Notifications.History (getNotifHistory, changeNotifHistory)
import Notifications.YTLivestream.Internal
  ( VideoIdExtraction (..)
  , getYtChannelPayload
  )

-- Downloaded libraries
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

type VideoId = Text

-- | This function only returns once Ado goes live on YouTube
getNextNewLivestream :: App VideoId
getNextNewLivestream = returnWhenFound newLivestream "New livestream"

-- | Returns the video ID of Ado's just-started livestream, or a Left explaining
-- what problem it encountered.
newLivestream :: App (Either Text VideoId)
newLivestream = getYtChannelPayload >>= \case
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
