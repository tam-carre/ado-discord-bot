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
import App.Lenses                  (authorName, color, content, description, embeds, fields,
                                    footerText, id, name, title, token, value, (%~), (.~), (?~),
                                    (^.))
import App.Utils                   (trunc, when')
import Data.Default                (Default (def))
import Discord                     (restCall)
import Discord.Interactions        (Interaction, InteractionResponse (..),
                                    InteractionResponseMessage (..), interactionResponseBasic)
import Discord.Requests            qualified as R
import Discord.Types               (ChannelId, CreateEmbed (..), DiscordColor (..))

----------------------------------------------------------------------------------------------------

instance Default InteractionResponseMessage where
  def = InteractionResponseMessage Nothing Nothing Nothing Nothing Nothing Nothing Nothing

reply ∷ Interaction → Text → App ()
reply intr = resp intr . interactionResponseBasic

replyEmbed ∷ Interaction → Text → App ()
replyEmbed intr msg =
  resp intr . InteractionResponseChannelMessage $ def & embeds ?~ [embed & description .~ msg]

embed ∷ CreateEmbed
embed = def & color ?~ DiscordColorDarkBlue

resp ∷ Interaction → InteractionResponse → App ()
resp intr = lift . restCall_ . R.CreateInteractionResponse (intr^.id) (intr^.token)

send ∷ Text → ChannelId → App ()
send content' cid = lift . restCall_ $ R.CreateMessage cid content'

-- | like `send` but accepts an unwrapped Word64
send' ∷ Text → Word64 → App ()
send' content' = send content' . w64DId

sendWithEmbed ∷ ChannelId → Text → CreateEmbed → App ()
sendWithEmbed cid txt emb = do
  result ← lift . restCall . R.CreateMessageDetailed cid $
    def & content .~ txt
        & embeds  ?~ [safelyTruncate emb]
  when' isLeft print result

safelyTruncate ∷ CreateEmbed → CreateEmbed
safelyTruncate
  = description %~ trunc 4096
  ⋙ title       %~ trunc 256
  ⋙ footerText  %~ trunc 2048
  ⋙ authorName  %~ trunc 256
  ⋙ fields      %~ take 25 . map (name %~ trunc 256 ⋙ value %~ trunc 1024)

sendWithEmbed' ∷ Word64 → Text → CreateEmbed → App ()
sendWithEmbed' = sendWithEmbed . w64DId
