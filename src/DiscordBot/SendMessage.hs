module DiscordBot.SendMessage (reply, replyEmbed, embed) where

-- Downloaded libraries
import Discord   (DiscordHandler, restCall, def)
import Data.Text (Text)

import qualified Discord.Requests as R

import Discord.Types
  ( InteractionId
  , InteractionToken
  , CreateEmbed (..)
  , DiscordColor (..)
  )
import Discord.Interactions
  ( interactionResponseBasic
  , InteractionResponse (InteractionResponseChannelMessage)
  , InteractionResponseMessage (..)
  )

-- Base
import Control.Monad (void)

-------------------------------------------------------------------------------

reply :: (InteractionId, InteractionToken) -> Text -> DiscordHandler ()
reply (iId, iToken) = resp iId iToken . interactionResponseBasic

replyEmbed :: (InteractionId, InteractionToken) -> Text -> DiscordHandler ()
replyEmbed (iId, iToken) msg =
    resp iId iToken
  . InteractionResponseChannelMessage
  $ intrRespMsg
      { interactionResponseMessageEmbeds =
        Just [embed { createEmbedDescription = msg }]
      }

embed :: CreateEmbed
embed = def { createEmbedColor = Just DiscordColorDarkBlue }

intrRespMsg :: InteractionResponseMessage
intrRespMsg = InteractionResponseMessage
  { interactionResponseMessageTTS             = Nothing
  , interactionResponseMessageContent         = Nothing
  , interactionResponseMessageEmbeds          = Nothing
  , interactionResponseMessageAllowedMentions = Nothing
  , interactionResponseMessageFlags           = Nothing
  , interactionResponseMessageComponents      = Nothing
  , interactionResponseMessageAttachments     = Nothing
  }

resp :: InteractionId -> InteractionToken -> InteractionResponse -> DiscordHandler ()
resp iId iToken = void . restCall . R.CreateInteractionResponse iId iToken
