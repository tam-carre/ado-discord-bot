module App.Discord.Events.Gateway.Ready (onReady) where

import App                         (App)
import App.BotConfig               (botConfig)
import App.Discord.Commands        (appCommands)
import App.Discord.Guilds.Settings (w64DId)
import App.Discord.Internal        (restCall_)
import App.Discord.SlashCommand    (SlashCommand (..))
import App.Lenses                  (id, ownerDebugGuildId, registration, to, (^.), (≫^.))
import Discord                     (RestCallErrorCode (..), restCall)
import Discord.Interactions        (ApplicationCommand (..))
import Discord.Requests            (ApplicationCommandRequest (..))
import Discord.Types               (ApplicationId, GuildId)

----------------------------------------------------------------------------------------------------

onReady ∷ ApplicationId → App ()
onReady appId = do
  echo "Bot ready!"

  appCmdRegistrations ← mapM (tryRegistering appId) appCommands

  case sequence appCmdRegistrations of
    Left _err  → echo "[!] Failed to register some commands (check API rules)"
    Right cmds → do
      echo $ "Registered " ⊕ show (length cmds) ⊕ " command(s)."
      unregisterOutdatedCmds appId cmds

tryRegistering ∷ ApplicationId → SlashCommand → App (Either RestCallErrorCode ApplicationCommand)
tryRegistering appId cmd = case cmd^.registration of
  Just reg → lift $ do
    restCall_ $ CreateGuildApplicationCommand appId debugGuild reg
    restCall  $ CreateGlobalApplicationCommand appId reg
  Nothing → pure . Left $ RestCallErrorCode 0 "" ""

unregisterOutdatedCmds ∷ ApplicationId → [ApplicationCommand] → App ()
unregisterOutdatedCmds appId validCmds = do
  registered ← lift . restCall $ GetGlobalApplicationCommands appId
  case registered of
    Left err → echo $ "Failed to get registered slash commands: " ⊕ show err
    Right cmds → do
      let outdatedIds = filter (∉ validCmds≫^.id) (cmds≫^.id)
      lift . forM_ outdatedIds $ \cmdId → do
        restCall_ $ DeleteGlobalApplicationCommand appId cmdId
        restCall_ $ DeleteGuildApplicationCommand appId debugGuild cmdId

debugGuild ∷ GuildId
debugGuild = botConfig^.ownerDebugGuildId.to w64DId
