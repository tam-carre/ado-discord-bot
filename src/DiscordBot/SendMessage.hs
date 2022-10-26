module DiscordBot.SendMessage
  ( reply
  , replyEmbed
  , embed
  , send
  , send'
  , sendWithEmbed
  , sendWithEmbed'
  ) where

-- Downloaded libraries
import Discord (DiscordHandler, restCall, def)
import Discord.Requests (MessageDetailedOpts (..))
import Discord.Types
  ( InteractionId
  , InteractionToken
  , CreateEmbed (..)
  , DiscordColor (..)
  , ChannelId
  , DiscordId (..)
  , Snowflake (..)
  )
import Discord.Interactions
  ( interactionResponseBasic
  , InteractionResponse (..)
  , InteractionResponseMessage (..)
  )
import qualified Discord.Requests as R

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

send :: Text -> ChannelId -> DiscordHandler ()
send content channelId = void . restCall $ R.CreateMessage channelId content

-- | like `send` but accepts an unwrapped Word64
send' :: Text -> Word64 -> DiscordHandler ()
send' content = send content . DiscordId . Snowflake

sendWithEmbed :: ChannelId -> Text -> CreateEmbed -> DiscordHandler ()
sendWithEmbed channelId txt emb = do
  result <- restCall . R.CreateMessageDetailed channelId $ def
    { messageDetailedContent = txt
    , messageDetailedEmbeds  = Just [emb]
    }
  if isLeft result
    then print result
    else pass

sendWithEmbed' :: Word64 -> Text -> CreateEmbed -> DiscordHandler ()
sendWithEmbed' = sendWithEmbed . DiscordId . Snowflake
