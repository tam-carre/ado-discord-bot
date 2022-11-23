module App.Discord.Commands (appCommands, cmdByName) where

import App.Discord.Perms.Types       (PermLvl (PermLvlUser))
import App.Discord.SendMessage
import App.Discord.SlashCommand      (SlashCommand (SlashCommand), chatInput, notificationChCmd,
                                      notificationChCmdNoRole, roleCmd)
import App.Lenses                    (_Just, channelId, communityPostCh, communityPostRole,
                                      description, embeds, modRole, name, relayCh, secretBaseCh,
                                      secretBaseRole, (.~), (?~), (^.), (^?))
import Data.Default                  (Default (..))
import Discord                       (RestCallErrorCode (RestCallErrorCode), restCall)
import Discord.Internal.Rest.Channel (ChannelRequest (CreateMessageDetailed))

----------------------------------------------------------------------------------------------------

cmdByName ∷ Text → Maybe SlashCommand
cmdByName cmdName = find ((cmdName ≡) . (^.name)) appCommands

appCommands ∷ [SlashCommand]
appCommands =
  [ communityCmd
  , secretBaseCmd
  , relayCmd
  , modRoleCmd
  , permtest
  ] where
  communityCmd =
    notificationChCmd "community"
      "Ado's community post"
      communityPostCh
      "will receive notifications for Ado's YouTube community posts"
      communityPostRole

  secretBaseCmd =
    notificationChCmd "secretbase"
      "Ado's Secret Base stream"
      secretBaseCh
      "will receive notifications for Ado's Secret Base streams/vids"
      secretBaseRole

  relayCmd =
    notificationChCmdNoRole "relay"
      "YouTube livechat TL and Ado's own message"
      relayCh

  modRoleCmd =
    roleCmd "modrole" "can manage this bot" modRole

  permtest =
    SlashCommand "permtest"
      PermLvlUser
      (chatInput "permtest" "Check if I have perms to send notifs in this channel" [])
      (\intr _mem _gid _opts → do
        result ← case intr^?channelId._Just of
          Nothing  → pure . Left $ RestCallErrorCode 99991 "No Channel ID" ""
          Just cid → lift . restCall . CreateMessageDetailed cid $
            def & embeds ?~ [embed & description .~ "If you can see this, permissions are OK."]

        case result of
          Left err → reply intr $ "Issue encountered " ⊕ show err
          Right _  → reply intr "Permissions are OK!"
      )
