{-# LANGUAGE LambdaCase, TypeApplications #-}

module App.Notifications.YTCommunityPosts (CommunityPost (..), getNextNewCommunityPost) where

import App                                         (App)
import App.Lenses                                  (community, date, id, none, to, (^.))
import App.Notifications.History                   (NotifHistoryDb, getNotifHistory)
import App.Notifications.Internal                  (addToHistory, returnWhenFound)
import App.Notifications.YTCommunityPosts.Internal (CommunityPost (..), getCommunityPostPayload)
import App.Utils                                   (decodeE, onFail, posit)
import Data.ByteString.Lazy.Char8                  qualified as L8
import Data.Text                                   (isInfixOf)

----------------------------------------------------------------------------------------------------

-- | This function only returns once Ado uploads a fresh new community post
getNextNewCommunityPost ∷ App CommunityPost
getNextNewCommunityPost = returnWhenFound latestCommunityPost "New community post"

-- | Returns a freshly uploaded community post by Ado, or a Left explaining
-- what problem it encountered.
latestCommunityPost ∷ App (Either Text CommunityPost)
latestCommunityPost = getPostId <$> getCommunityPostPayload <*> getNotifHistory ≫= \case
  Left e     → pure . Left $ "[Community] " ⊕ e
  Right post → Right post <$ addToHistory community (post^.id)

getPostId ∷ (Int, Maybe L8.ByteString) → NotifHistoryDb → Either Text CommunityPost
getPostId (status, payloadMaybe) history = do
  _       ← posit (status ≡ 200) "Non-200 status code"
  payload ← payloadMaybe & onFail "Failed to' extract JSON from HTML"
  post    ← decodeE @CommunityPost payload
  _       ← posit (post^.date.to isToday) "Post older than an hour"
  _       ← posit ((post^.id) ∉ history^.community) "Post already notified"
  pure post

isToday ∷ Text → Bool
isToday date' = none (`isInfixOf` date') ["day", "week", "month", "year", "hour"]
