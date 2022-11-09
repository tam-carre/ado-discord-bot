{-# LANGUAGE OverloadedStrings #-}

module Notifications.YTCommunityPosts.Internal
  ( CommunityPost (..)
  , extractData
  ) where

import Data.Aeson (Value (..))
import Json       ((?.), (?!!), unArr, unStr)
import qualified Data.Vector as Vector

-------------------------------------------------------------------------------

data CommunityPost = CommunityPost
  { author  :: Text
  , avatar  :: Text
  , postId  :: Text
  , date    :: Text
  , content :: Text
  } deriving (Show, Eq)

extractData :: Value -> Either Text CommunityPost
extractData ytData = do
  tabs <- pure ytData
    ?. "contents"
    ?. "twoColumnBrowseResultsRenderer"
    ?. "tabs"
   >>= unArr

  communityTab <-
    case
      Vector.find
        (\t -> pure t ?. "tabRenderer" ?. "title" == Right "Community")
        tabs
    of
      Nothing -> Left $ "Could not find community tab in "<> show tabs
      Just t  -> Right t

  latestPost <- pure communityTab
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
