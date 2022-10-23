module Prelude (module Relude, echo, tap) where

import Relude hiding (id)

-------------------------------------------------------------------------------

-- | Short name for putTextLn
echo :: MonadIO m => Text -> m ()
echo = putTextLn

tap :: MonadIO m => (a -> m ()) -> m a -> m a
tap sideEffect mainComputation = do
  result <- mainComputation
  sideEffect result
  pure result
