{-# LANGUAGE LambdaCase, TypeApplications #-}

module App.Notifications.YTLivestream (VideoId, getNextNewLivestream) where

import App                                     (App)
import App.Lenses                              (ytStream, (^.))
import App.Notifications.History               (getNotifHistory)
import App.Notifications.Internal              (addToHistory, returnWhenFound)
import App.Notifications.YTLivestream.Internal (VideoIdExtraction (..), getYtChannelPayload)
import Data.Aeson                              (eitherDecode)

----------------------------------------------------------------------------------------------------

type VideoId = Text

-- | This function only returns once Ado goes live on YouTube
getNextNewLivestream ∷ App VideoId
getNextNewLivestream = returnWhenFound newLivestream "New livestream"

-- | Returns the video ID of Ado's just-started livestream, or a Left explaining
-- what problem it encountered.
newLivestream ∷ App (Either Text VideoId)
newLivestream = getYtChannelPayload ≫= \case
  (200, Just payload) →
    case eitherDecode @VideoIdExtraction payload of
      Right (VideoId vidId) → do
        notifHistory ← getNotifHistory
        if vidId ∉ notifHistory^.ytStream
          then addToHistory ytStream vidId $> Right vidId

          else err "Found livestream already notified"
      Right NotLive → err "Found stream but not live yet"
      Left e        → err $ "Failed to extract: " ⊕ show e
  (200, Nothing) → err "No ongoing live"
  _              → err "Non-200 status code"
  where err  = pure . Left . ("[Livestream] " ⊕)
