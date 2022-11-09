{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Notifications.YTCommunityPosts
  ( CommunityPost (..)
  , getNextNewCommunityPost
  ) where

-- Ado Bot modules
import Notifications.YTCommunityPosts.Internal (CommunityPost (..), extractData)
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
import Data.Aeson (Value (..), decode)
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
  request <- liftIO $ get' url
  response <- httpBS . eng $ request

  let status  = getResponseStatusCode response
      jsonStr = getPayload . decodeUtf8 $ getResponseBody response

  case (status, jsonStr) of
    (200, Just payload) -> case decode payload :: Maybe Value of
      Just (extractData -> Right post@(CommunityPost { postId, date })) ->
        if isToday date then do
          notifHistory <- getNotifHistory db
          if postId `notElem` notifHistory.community then do
            changeNotifHistory db
              (\hist -> hist { community = postId : take 50 hist.community })

            pure $ Right post

          else err "Found recent post already notified"

        else err "Found post but it's older than an hour"

      Just (extractData -> Left e) -> err $ "Failed to extract: " <> show e
      _                            -> err "Found JSON but failed to decode"

    (200, Nothing) -> err "Failed to extract JSON from HTML source"
    _              -> err "Non-200 status code"

  where
  url  = "https://www.youtube.com/c/Ado1024/community"
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
