{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}

module Notifications.SecretBase
  ( SecretBaseLive (..)
  , SecretBaseVid (..)
  , getNextNewSecretBaseStream
  , getNextNewSecretBaseVid
  ) where

-- Ado Bot modules
import Lenses
import App                   (App)
import Network               (fetchJson)
import Utils                 ((>>>=))
import Notifications.Utils   (returnWhenFound)
import Notifications.History (getNotifHistory, changeNotifHistory)
import Notifications.SecretBase.Internal
  ( Lives (..)
  , SecretBaseLive (..)
  , SecretBaseVid (..)
  , Vids (..)
  , secretBaseVidsApi
  , secretBaseStreamsApi
  )

-- Downloaded libraries
import Data.Time (getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)

-------------------------------------------------------------------------------

-- | This function only returns once Ado goes live on Secret Base
getNextNewSecretBaseStream :: App SecretBaseLive
getNextNewSecretBaseStream = returnWhenFound latestSecretBaseStream "New Secret Base"

-- | This function only returns once Ado uploads a video on Secret Base
getNextNewSecretBaseVid :: App SecretBaseVid
getNextNewSecretBaseVid = returnWhenFound latestSecretBaseVid "New Secret Base"

-- | Returns a freshly started Secret Base stream by Ado, or a Left explaining
-- what problem it encountered.
latestSecretBaseStream :: App (Either Text SecretBaseLive)
latestSecretBaseStream = fetchJson @Lives secretBaseStreamsApi >>>= \case
  Lives [] -> err "No ongoing live"
  Lives lives -> do
    notifHistory <- getNotifHistory
    let new = filter ((`notElem` (notifHistory^.sbStream)) . view url) lives
    case listToMaybe new of
      Nothing -> err "Ongoing live found but already notified"
      Just live -> do
        changeNotifHistory . over sbStream $ \hist -> (live^.url) : take 50 hist
        pure $ Right live
  where
  err = pure . Left . ("[Secret Base Stream] " <>)

-- | Returns a freshly uploaded Secret Base video by Ado, or a Left explaining
-- what problem it encountered.
latestSecretBaseVid :: App (Either Text SecretBaseVid)
latestSecretBaseVid = fetchJson @Vids secretBaseVidsApi >>>= \(Vids vids) -> do
  currTime <- liftIO getCurrentTime
  let isNew vid = 300 > nominalDiffTimeToSeconds (diffUTCTime currTime (vid^.date))
  notifHistory <- getNotifHistory
  let notNotified = filter ((`notElem` (notifHistory^.sbVid)) . view url) vids
  let new = filter isNew notNotified
  case listToMaybe new of
    Nothing -> err "Recent videos not fresh or already notified"
    Just video -> do
      changeNotifHistory . over sbVid $ \hist -> (video^.url) : take 50 hist
      pure $ Right video
  where
  err = pure . Left . ("[Secret Base Vid] " <>)
