{-# OPTIONS -Wno-missing-fields #-}
{-# OPTIONS -Wno-orphans #-}

module App.Discord.SendMessage
  ( embed
  , reply
  , replyEmbed
  , send
  , send'
  , sendWithEmbed
  , sendWithEmbed'
  ) where

import App                         (App)
import App.Discord.Guilds.Settings (w64DId)
import App.Discord.Internal        (restCall_)
import App.Lenses                  (color, content, description, embeds, (.~), (?~))
import App.Utils                   (when')
import Data.Default                (Default (def))
import Discord                     (restCall)
import Discord.Interactions        (InteractionResponse (..), InteractionResponseMessage (..),
                                    interactionResponseBasic)
import Discord.Requests            qualified as R
import Discord.Types               (ChannelId, CreateEmbed (..), DiscordColor (..), InteractionId,
                                    InteractionToken)

----------------------------------------------------------------------------------------------------

instance Default InteractionResponseMessage where
  def = InteractionResponseMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing

reply ∷ (InteractionId, InteractionToken) → Text → App ()
reply (iId, iToken) = resp iId iToken . interactionResponseBasic

replyEmbed ∷ (InteractionId, InteractionToken) → Text → App ()
replyEmbed (iId, iTok) msg =
  resp iId iTok . InteractionResponseChannelMessage $ def & embeds ?~ [embed & description .~ msg]

embed ∷ CreateEmbed
embed = def & color ?~ DiscordColorDarkBlue

resp ∷ InteractionId → InteractionToken → InteractionResponse → App ()
resp iId iToken = lift . restCall_ . R.CreateInteractionResponse iId iToken

send ∷ Text → ChannelId → App ()
send content' cid = lift . restCall_ $ R.CreateMessage cid content'

-- | like `send` but accepts an unwrapped Word64
send' ∷ Text → Word64 → App ()
send' content' = send content' . w64DId

sendWithEmbed ∷ ChannelId → Text → CreateEmbed → App ()
sendWithEmbed cid txt emb = do
  result ← lift . restCall . R.CreateMessageDetailed cid $ def & content .~ txt
                                                               & embeds  ?~ [emb]
  when' isLeft print result

sendWithEmbed' ∷ Word64 → Text → CreateEmbed → App ()
sendWithEmbed' = sendWithEmbed . w64DId
