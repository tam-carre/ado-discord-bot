{-# OPTIONS -Wno-missing-fields #-}

module DiscordBot.SendMessage
  ( reply
  , replyEmbed
  , embed
  , send
  , send'
  , sendWithEmbed
  , sendWithEmbed'
  ) where

-- Ado Bot modules
import Lenses

-- Downloaded libraries
import Discord (DiscordHandler, restCall, def)
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
intrRespMsg =
  InteractionResponseMessage
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

resp :: InteractionId -> InteractionToken -> InteractionResponse -> DiscordHandler ()
resp iId iToken = void . restCall . R.CreateInteractionResponse iId iToken

send :: Text -> ChannelId -> DiscordHandler ()
send content' cid = void . restCall $ R.CreateMessage cid content'

-- | like `send` but accepts an unwrapped Word64
send' :: Text -> Word64 -> DiscordHandler ()
send' content' = send content' . DiscordId . Snowflake

sendWithEmbed :: ChannelId -> Text -> CreateEmbed -> DiscordHandler ()
sendWithEmbed cid txt emb = do
  result <- restCall . R.CreateMessageDetailed cid $ def & content .~ txt
                                                         & embeds  ?~ [emb]
  if isLeft result
    then print result
    else pass

sendWithEmbed' :: Word64 -> Text -> CreateEmbed -> DiscordHandler ()
sendWithEmbed' = sendWithEmbed . DiscordId . Snowflake
