module Utils (tap, betweenSubstrs, decorate, sleep, (>>>=), (<&>>=)) where

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

-- | e.g. you want to chain the `a` in `IO (Maybe a)` to another `IO (Maybe a)`
-- or if you're using Network.fetchJson you want to chain the `a` in
-- `MonadIO m => m (Either e a)` into another `m (Either e a)`
(>>>=) :: (Monad m1, Monad m2, Traversable m2)
  => m1 (m2 a)
  -> (a -> m1 (m2 b))
  -> m1 (m2 b)
m1 >>>= f = m1 >>= (fmap join . mapM f)

-- | e.g. you want to chain the `a`in `IO (Maybe a)` to a `Maybe b`
(<&>>=) :: (Monad m1, Monad m2) => m1 (m2 a) -> (a -> m2 b) -> m1 (m2 b)
m1 <&>>= f = m1 <&> (>>= f)
