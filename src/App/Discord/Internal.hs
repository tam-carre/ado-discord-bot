module App.Discord.Internal (restCall_) where

import Discord (DiscordHandler, FromJSON, Request, restCall)

----------------------------------------------------------------------------------------------------

restCall_ ∷ (Request (r a), FromJSON a) ⇒ r a → DiscordHandler ()
restCall_ = void . restCall
