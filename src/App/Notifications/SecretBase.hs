{-# LANGUAGE FlexibleContexts, LambdaCase, TypeApplications #-}

module App.Notifications.SecretBase
  ( SecretBaseLive (..)
  , SecretBaseVid (..)
  , getNextNewSecretBaseStream
  , getNextNewSecretBaseVid
  ) where

import App                                   (App)
import App.Lenses                            (date, sbStream, sbVid, url, (^.))
import App.Network                           (fetchJson)
import App.Notifications.History             (getNotifHistory)
import App.Notifications.Internal            (addToHistory, returnWhenFound)
import App.Notifications.SecretBase.Internal (Lives (..), SecretBaseLive (..), SecretBaseVid (..),
                                              Vids (..), secretBaseStreamsApi, secretBaseVidsApi)
import App.Utils                             ((>>>=))
import Data.Time                             (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)

----------------------------------------------------------------------------------------------------

-- | This function only returns once Ado goes live on Secret Base
getNextNewSecretBaseStream ∷ App SecretBaseLive
getNextNewSecretBaseStream = returnWhenFound latestSecretBaseStream "New Secret Base"

-- | This function only returns once Ado uploads a video on Secret Base
getNextNewSecretBaseVid ∷ App SecretBaseVid
getNextNewSecretBaseVid = returnWhenFound latestSecretBaseVid "New Secret Base"

-- | Returns a freshly started Secret Base stream by Ado, or a Left explaining
-- what problem it encountered.
latestSecretBaseStream ∷ App (Either Text SecretBaseLive)
latestSecretBaseStream = fetchJson @Lives secretBaseStreamsApi >>>= \case
  Lives [] → err "No ongoing live"
  Lives lives → do
    notifHistory ← getNotifHistory
    let new = filter ((∉ notifHistory^.sbStream) . (^.url)) lives
    case listToMaybe new of
      Nothing → err "Ongoing live found but already notified"
      Just live → do
        addToHistory sbStream (live^.url)
        pure $ Right live
  where
  err = pure . Left . ("[Secret Base Stream] " ⊕)

-- | Returns a freshly uploaded Secret Base video by Ado, or a Left explaining
-- what problem it encountered.
latestSecretBaseVid ∷ App (Either Text SecretBaseVid)
latestSecretBaseVid = fetchJson @Vids secretBaseVidsApi >>>= \(Vids vids) → do
  currTime ← liftIO getCurrentTime
  let isNew vid = 300 > nominalDiffTimeToSeconds (diffUTCTime currTime (vid^.date))
  notifHistory ← getNotifHistory
  let notNotified = filter ((∉ notifHistory^.sbVid) . (^.url)) vids
  let new = filter isNew notNotified
  case listToMaybe new of
    Nothing    → err "Recent videos not fresh or already notified"
    Just video → Right video <$ addToHistory sbVid (video^.url)
  where
  err = pure . Left . ("[Secret Base Vid] " ⊕)
