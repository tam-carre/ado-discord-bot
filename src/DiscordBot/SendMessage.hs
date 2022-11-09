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
import App    (App)

-- Downloaded libraries
import Discord (restCall, def)
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

reply :: (InteractionId, InteractionToken) -> Text -> App ()
reply (iId, iToken) = resp iId iToken . interactionResponseBasic

replyEmbed :: (InteractionId, InteractionToken) -> Text -> App ()
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

resp :: InteractionId -> InteractionToken -> InteractionResponse -> App ()
resp iId iToken = lift . void . restCall . R.CreateInteractionResponse iId iToken

send :: Text -> ChannelId -> App ()
send content' cid = lift . void . restCall $ R.CreateMessage cid content'

-- | like `send` but accepts an unwrapped Word64
send' :: Text -> Word64 -> App ()
send' content' = send content' . DiscordId . Snowflake

sendWithEmbed :: ChannelId -> Text -> CreateEmbed -> App ()
sendWithEmbed cid txt emb = do
  result <- lift . restCall . R.CreateMessageDetailed cid $ def & content .~ txt
                                                                & embeds  ?~ [emb]
  if isLeft result
    then print result
    else pass

sendWithEmbed' :: Word64 -> Text -> CreateEmbed -> App ()
sendWithEmbed' = sendWithEmbed . DiscordId . Snowflake
