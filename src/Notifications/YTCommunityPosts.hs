{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}

module Notifications.YTCommunityPosts
  ( CommunityPost (..)
  , getNextNewCommunityPost
  ) where

-- Ado Bot modules
import Lenses
import App                                     (App)
import Notifications.YTCommunityPosts.Internal (CommunityPost (..), getCommunityPostPayload)
import Notifications.Utils                     (returnWhenFound)
import Notifications.History                   (getNotifHistory , changeNotifHistory)

-- Downloaded libraries
import Data.Text  (isInfixOf)
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

-- | This function only returns once Ado uploads a fresh new community post
getNextNewCommunityPost :: App CommunityPost
getNextNewCommunityPost = returnWhenFound latestCommunityPost "New community post"

-- | Returns a freshly uploaded community post by Ado, or a Left explaining
-- what problem it encountered.
latestCommunityPost :: App (Either Text CommunityPost)
latestCommunityPost = getCommunityPostPayload >>= \case
  (200, Just payload) -> case eitherDecode @CommunityPost payload of
    Right post ->
      if isToday (post^.date) then do
        notifHistory <- getNotifHistory
        if (post^.id) `notElem` (notifHistory^.community) then do
          changeNotifHistory . over community $ \h -> (post^.id) : take 50 h

          pure $ Right post

        else err "Found recent post already notified"

      else err "Found post but it's older than an hour"

    Left e -> err $ "Failed to extract: " <> show e

  (200, Nothing) -> err "Failed to extract JSON from HTML source"
  _              -> err "Non-200 status code"
  where
  err  = pure . Left . ("[Community] " <>)

-- | Assesses a YouTube English-language relative datestring e.g. "1 hour ago"
isToday :: Text -> Bool
isToday date' =
  all (\unit -> not $ unit `isInfixOf` date') ["day", "week", "month", "year", "hour"]
