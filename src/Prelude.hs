module Prelude (module Relude, echo) where

import Relude

-------------------------------------------------------------------------------

-- | Short name for putTextLn
echo :: MonadIO m => Text -> m ()
echo = putTextLn
