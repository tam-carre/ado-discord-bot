{-# LANGUAGE LambdaCase, TypeApplications #-}

module App.Notifications.YTLivestream (VideoId, getNextNewLivestream) where

import App                                     (App)
import App.Lenses                              (_VideoId, ytStream, (^.), (^?))
import App.Notifications.History               (NotifHistoryDb, getNotifHistory)
import App.Notifications.Internal              (addToHistory, returnWhenFound)
import App.Notifications.YTLivestream.Internal (VideoIdExtraction (..), getYtChannelPayload)
import App.Utils                               (decodeE, onFail, posit)
import Data.ByteString.Lazy.Char8              qualified as L8

----------------------------------------------------------------------------------------------------

type VideoId = Text

-- | This function only returns once Ado goes live on YouTube
getNextNewLivestream ∷ App VideoId
getNextNewLivestream = returnWhenFound newLivestream "New livestream"

-- | Returns the video ID of Ado's just-started livestream, or a Left explaining
-- what problem it encountered.
newLivestream ∷ App (Either Text VideoId)
newLivestream = getVidId <$> getYtChannelPayload <*> getNotifHistory ≫= \case
  Left e      → pure . Left $ "[YTLivestream] " ⊕ e
  Right vidId → Right vidId <$ addToHistory ytStream vidId

getVidId ∷ (Int, Maybe L8.ByteString) → NotifHistoryDb → Either Text VideoId
getVidId (status, payloadMaybe) history = do
  _          ← posit (status ≡ 200) "Non-200 status code"
  payload    ← payloadMaybe & onFail "No ongoing live"
  extraction ← decodeE @VideoIdExtraction payload
  vidId      ← extraction^?_VideoId & onFail "No ongoing live"
  _          ← posit (vidId ∉ history^.ytStream) "Already notified live"
  pure vidId
