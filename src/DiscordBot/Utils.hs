module DiscordBot.Utils
  ( putLn
  ) where

-- Downloaded libraries
import Discord   (DiscordHandler)
import UnliftIO  (liftIO)
import Data.Text (Text)

import qualified Data.Text.IO as TIO

-------------------------------------------------------------------------------

putLn :: Text -> DiscordHandler ()
putLn = liftIO . TIO.putStrLn
