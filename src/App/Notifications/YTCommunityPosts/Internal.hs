module App.Notifications.YTCommunityPosts.Internal
  ( CommunityPost (..)
  , getCommunityPostPayload
  ) where

import App.Utils                  (betweenSubstrs)
import Control.Lens               ((^.), (^..), (^?))
import Data.Aeson                 (FromJSON (..))
import Data.Aeson.Lens            (_String, key, nth, values)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text.Lazy.Encoding    qualified as Lazy.Encoding
import Network.HTTP.Simple        (getResponseBody, getResponseStatusCode, httpBS, parseRequest,
                                   setRequestHeader)

----------------------------------------------------------------------------------------------------

data CommunityPost
  = CommunityPost
    { _author  ∷ Text
    , _avatar  ∷ Text
    , _id      ∷ Text
    , _date    ∷ Text
    , _content ∷ Text
    }
  deriving (Eq, Show)

instance FromJSON CommunityPost where
  parseJSON ytData = do
    let tabs = ytData^..key "contents".key "twoColumnBrowseResultsRenderer".key "tabs".values
        isCommunity t = t^.key "tabRenderer".key "title"._String ≡ "Community"

    communityTab ← case find isCommunity tabs of
      Nothing → fail $ "Coult not find community tab in " ⊕ show tabs
      Just t  → pure t

    let latestPost = communityTab^?key "tabRenderer"
                                  .key "content"
                                  .key "sectionListRenderer"
                                  .key "contents".nth 0
                                  .key "itemSectionRenderer"
                                  .key "contents".nth 0
                                  .key "backstagePostThreadRenderer"
                                  .key "post"
                                  .key "backstagePostRenderer"

    p ← maybe (fail "Could not find latest post") pure latestPost

    pure $ CommunityPost
      { _author  = p^.key "authorText".key "runs".nth 0.key "text"._String
      , _avatar  = "https:" ⊕ p^.key "authorThumbnail".key "thumbnails".nth 2.key "url"._String
      , _id      = p^.key "postId"._String
      , _date    = p^.key "publishedTimeText".key "runs".nth 0.key "text"._String
      , _content = foldr ((⊕) . (^.key "text"._String)) "" $
                     p^..key "contentText".key "runs".values
      }

getCommunityPostPayload ∷ MonadIO m ⇒ m (Int, Maybe L8.ByteString)
getCommunityPostPayload = do
  request ← liftIO $ get' "https://www.youtube.com/c/Ado1024/community"
  response ← httpBS . eng $ request

  let status  = getResponseStatusCode response
      jsonStr = getPayload . decodeUtf8 $ getResponseBody response

  pure (status, jsonStr)
  where
  eng  = setRequestHeader "Accept-Language" ["en"]
  get' = parseRequest . ("GET " <>)
  -- | Takes the page source, returns only the embedded JSON
  getPayload = fmap (Lazy.Encoding.encodeUtf8 . fromStrict)
    . betweenSubstrs "var ytInitialData = " ";</script>"
