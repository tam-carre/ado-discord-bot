{-# LANGUAGE LambdaCase, TypeApplications #-}

module App.Notifications.YTCommunityPosts (CommunityPost (..), getNextNewCommunityPost) where

import App                                         (App)
import App.Lenses                                  (community, date, id, none, (^.) )
import App.Notifications.History                   (getNotifHistory)
import App.Notifications.Internal                  (addToHistory, returnWhenFound)
import App.Notifications.YTCommunityPosts.Internal (CommunityPost (..), getCommunityPostPayload)
import Data.Aeson                                  (eitherDecode)
import Data.Text                                   (isInfixOf)

----------------------------------------------------------------------------------------------------

-- | This function only returns once Ado uploads a fresh new community post
getNextNewCommunityPost ∷ App CommunityPost
getNextNewCommunityPost = returnWhenFound latestCommunityPost "New community post"

-- | Returns a freshly uploaded community post by Ado, or a Left explaining
-- what problem it encountered.
latestCommunityPost ∷ App (Either Text CommunityPost)
latestCommunityPost = getCommunityPostPayload ≫= \case
  (200, Just payload) → case eitherDecode @CommunityPost payload of
    Right post →
      if isToday (post^.date) then do
        notifHistory ← getNotifHistory
        if (post^.id) ∉ (notifHistory^.community)
          then Right post <$ addToHistory community (post^.id)
          else err "Found recent post already notified"
      else err "Found post but it's older than an hour"
    Left e → err $ "Failed to extract: " ⊕ show e
  (200, Nothing) → err "Failed to extract JSON from HTML source"
  _              → err "Non-200 status code"
  where
  err           = pure . Left . ("[Community] " ⊕)
  isToday date' = none (`isInfixOf` date') ["day", "week", "month", "year", "hour"]
