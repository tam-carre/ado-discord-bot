{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Notifications.YouTubeCommunityPosts.Watcher
  ( CommunityPost (..)
  , getNextNewCommunityPost
  ) where

-- Ado Bot modules
import Json  ((?.), (?!!), unArr, unStr)
import Utils (betweenSubstrs, sleep)
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
import qualified Data.Vector                as Vector
import qualified Data.ByteString.Lazy.Char8 as L8

-------------------------------------------------------------------------------

data CommunityPost = CommunityPost
  { author  :: Text
  , avatar  :: Text
  , postId  :: Text
  , date    :: Text
  , content :: Text
  }

-- | This function only returns once Ado uploads a fresh new community post
getNextNewCommunityPost :: AcidState NotifHistoryDb -> IO CommunityPost
getNextNewCommunityPost db = latestCommunityPost db >>= \case
  Right postData -> do
    echo "Found a new community post."
    pure postData
  Left err -> do
    echo err
    sleep 30
    getNextNewCommunityPost db

-- | Returns a freshly uploaded community post by Ado, or a Left explaining
-- what problem it encountered.
latestCommunityPost :: AcidState NotifHistoryDb -> IO (Either Text CommunityPost)
latestCommunityPost db = do
  response <- httpBS . eng =<< get' url

  let status  = getResponseStatusCode response
  let jsonStr = getPayload . decodeUtf8 $ getResponseBody response

  case (status, jsonStr) of
    (200, Just payload) -> case decode payload :: Maybe Value of
      Just (extractData -> Just post@(CommunityPost { postId, date })) ->
        if isToday date then do
          notifHistory <- getNotifHistory db
          if postId `notElem` notifHistory.community then do
            changeNotifHistory db
              (\hist -> hist { community = postId : take 50 hist.community })

            pure $ Right post

          else err "Found recent post already notified"

        else err "Found post but it's older than a day"

      Just (extractData -> Nothing) -> err "Found JSON but couldn't extract post"
      _                             -> err "Found JSON but failed to decode"

    (200, Nothing) -> err "Failed to extract JSON from HTML source"
    _              -> err "Non-200 status code for community post page"

  where
  url  = "https://www.youtube.com/c/Ado1024/community"
  eng  = setRequestHeader "Accept-Language" ["en"]
  get' = parseRequest . ("GET " <>)
  err  = pure . Left

-- | Assesses a YouTube English-language relative datestring e.g. "1 hour ago"
isToday :: Text -> Bool
isToday date' =
  all (\unit -> not $ unit `isInfixOf` date') ["day", "week", "month", "year"]

-- | Takes the page source, returns only the embedded JSON as ByteString
getPayload :: Text -> Maybe L8.ByteString
getPayload = fmap (Lazy.Encoding.encodeUtf8 . fromStrict)
  . betweenSubstrs "var ytInitialData = " ";</script>"

-- | Get the (id, date, content) out of the JSON payload
extractData :: Value -> Maybe CommunityPost
extractData ytData = do
  latestPost <- Just ytData
    ?. "contents"
    ?. "twoColumnBrowseResultsRenderer"
    ?. "tabs" ?!! 3
    ?. "tabRenderer"
    ?. "content"
    ?. "sectionListRenderer"
    ?. "contents" ?!! 0
    ?. "itemSectionRenderer"
    ?. "contents" ?!! 0
    ?. "backstagePostThreadRenderer"
    ?. "post"
    ?. "backstagePostRenderer"

  content' <- Just latestPost
    ?. "contentText"
    ?. "runs"
   >>= unArr
   >>= traverse (\el -> Just el ?. "text" >>= unStr)
   <&> Vector.foldr (<>) ""

  -- | relative date e.g. "1 hour ago"
  date' <- Just latestPost
    ?. "publishedTimeText"
    ?. "runs" ?!! 0
    ?. "text"
   >>= unStr

  postId' <- Just latestPost ?. "postId" >>= unStr

  author' <- Just latestPost ?. "authorText" ?. "runs" ?!! 0 ?. "text" >>= unStr

  avatar' <- Just latestPost
    ?. "authorThumbnail"
    ?. "thumbnails" ?!! 2
    ?. "url"
   >>= unStr
   <&> ("https:" <>)

  pure $ CommunityPost
    { author  = author'
    , avatar  = avatar'
    , postId  = postId'
    , date    = date'
    , content = content'
    }
