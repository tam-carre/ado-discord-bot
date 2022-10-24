module Utils (tap, betweenSubstrs, decorate, sleep) where

-- Downloaded libraries
import Data.Text  (splitOn)

-- Base
import Control.Concurrent (threadDelay)

-------------------------------------------------------------------------------

-- | Add a side effect to an IO action
tap :: MonadIO m => (a -> m ()) -> m a -> m a
tap sideEffect mainComputation = do
  result <- mainComputation
  sideEffect result
  pure result

-- | e.g. `betweenSubstrs "foo" "bar" "hi foo quz bar hi" == Just " quz "`
betweenSubstrs :: Text -> Text -> Text -> Maybe Text
betweenSubstrs substr1 substr2 =
  maybeAt 0 <=< fmap (splitOn substr2) . maybeAt 1 . splitOn substr1

-- | e.g. `decorate "<" ">" "script" == "<script>"
decorate :: Text -> Text -> Text -> Text
decorate left right input = left <> input <> right

-- | takes seconds as argument
sleep :: MonadIO m => Int -> m ()
sleep = liftIO . threadDelay . (* 1000000)
