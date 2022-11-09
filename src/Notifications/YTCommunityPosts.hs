{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeApplications    #-}

module Notifications.YTCommunityPosts
  ( CommunityPost (..)
  , getNextNewCommunityPost
  ) where

-- Ado Bot modules
import Lenses
import Notifications.YTCommunityPosts.Internal (CommunityPost (..))
import Utils                                   (betweenSubstrs)
import Notifications.Utils                     (returnWhenFound)
import Notifications.History
  ( NotifHistoryDb (..)
  , getNotifHistory
  , changeNotifHistory
  )

-- Downloaded libraries
import Data.Acid  (AcidState)
import Data.Text  (isInfixOf)
import Data.Aeson (eitherDecode)
import Network.HTTP.Simple
  ( parseRequest
  , getResponseStatusCode
  , getResponseBody
  , httpBS
  , setRequestHeader
  )
import qualified Data.Text.Lazy.Encoding    as Lazy.Encoding
import qualified Data.ByteString.Lazy.Char8 as L8

-------------------------------------------------------------------------------

-- | This function only returns once Ado uploads a fresh new community post
getNextNewCommunityPost :: MonadIO m => AcidState NotifHistoryDb -> m CommunityPost
getNextNewCommunityPost = returnWhenFound latestCommunityPost "New community post"

-- | Returns a freshly uploaded community post by Ado, or a Left explaining
-- what problem it encountered.
latestCommunityPost :: MonadIO m => AcidState NotifHistoryDb -> m (Either Text CommunityPost)
latestCommunityPost db = do
  request <- liftIO $ get' "https://www.youtube.com/c/Ado1024/community"
  response <- httpBS . eng $ request

  let status  = getResponseStatusCode response
      jsonStr = getPayload . decodeUtf8 $ getResponseBody response

  case (status, jsonStr) of
    (200, Just payload) -> case eitherDecode @CommunityPost payload of
      Right post@(CommunityPost { _postId, _date }) ->
        if isToday _date then do
          notifHistory <- getNotifHistory db
          if _postId `notElem` (notifHistory^.community) then do
            changeNotifHistory db . over community $ \h -> _postId : take 50 h

            pure $ Right post

          else err "Found recent post already notified"

        else err "Found post but it's older than an hour"

      Left e -> err $ "Failed to extract: " <> show e

    (200, Nothing) -> err "Failed to extract JSON from HTML source"
    _              -> err "Non-200 status code"

  where
  eng  = setRequestHeader "Accept-Language" ["en"]
  get' = parseRequest . ("GET " <>)
  err  = pure . Left . ("[Community] " <>)

-- | Assesses a YouTube English-language relative datestring e.g. "1 hour ago"
isToday :: Text -> Bool
isToday date' =
  all (\unit -> not $ unit `isInfixOf` date') ["day", "week", "month", "year", "hour"]

-- | Takes the page source, returns only the embedded JSON as ByteString
getPayload :: Text -> Maybe L8.ByteString
getPayload = fmap (Lazy.Encoding.encodeUtf8 . fromStrict)
  . betweenSubstrs "var ytInitialData = " ";</script>"
