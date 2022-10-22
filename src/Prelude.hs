module Prelude (module Relude, echo) where

import Relude hiding (id)

-------------------------------------------------------------------------------

-- | Short name for putTextLn
echo :: MonadIO m => Text -> m ()
echo = putTextLn
