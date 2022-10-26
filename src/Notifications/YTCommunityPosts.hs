{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Notifications.YTCommunityPosts
  ( CommunityPost (..)
  , getNextNewCommunityPost
  ) where

-- Ado Bot modules
import Json                ((?.), (?!!), unArr, unStr)
import Utils               (betweenSubstrs)
import Notifications.Utils (returnWhenFound)
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

        else err "Found post but it's older than a day"

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
  all (\unit -> not $ unit `isInfixOf` date') ["day", "week", "month", "year"]

-- | Takes the page source, returns only the embedded JSON as ByteString
getPayload :: Text -> Maybe L8.ByteString
getPayload = fmap (Lazy.Encoding.encodeUtf8 . fromStrict)
  . betweenSubstrs "var ytInitialData = " ";</script>"

extractData :: Value -> Either Text CommunityPost
extractData ytData = do
  latestPost <- pure ytData
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

  content' <- pure latestPost
    ?. "contentText"
    ?. "runs"
   >>= unArr
   >>= traverse (\el -> pure el ?. "text" >>= unStr)
   <&> Vector.foldr (<>) ""

  -- | relative date e.g. "1 hour ago"
  date' <- pure latestPost
    ?. "publishedTimeText"
    ?. "runs" ?!! 0
    ?. "text"
   >>= unStr

  postId' <- pure latestPost ?. "postId" >>= unStr

  author' <- pure latestPost ?. "authorText" ?. "runs" ?!! 0 ?. "text" >>= unStr

  avatar' <- pure latestPost
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
