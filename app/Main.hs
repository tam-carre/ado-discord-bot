module Main (main) where

import App                                       (Env (Env))
import App.BotConfig                             (botConfig)
import App.Discord.Events                        (onDiscordEvent)
import App.Discord.Events.BotStartedRunNotifiers (onBotStartedRunNotifiers)
import App.Discord.Guilds.Settings               (getSettingsDb)
import App.Lenses                                (botToken, gatewayIntent, messageContent, onEvent,
                                                  onStart, token, (.~), (^.))
import App.Notifications.History                 (getNotifHistoryDb)
import Discord                                   (def, runDiscord)

----------------------------------------------------------------------------------------------------

main ∷ IO ()
main = do
  echo "Bot started."

  withEnv ← usingReaderT <$> (Env <$> getNotifHistoryDb <*> getSettingsDb)

  botTerminationError ←
    runDiscord $ def & token   .~ botConfig^.botToken
                     & onEvent .~ withEnv . onDiscordEvent
                     & onStart .~ withEnv onBotStartedRunNotifiers
                     & gatewayIntent .~ (def & messageContent .~ False)

  echo $ "A fatal error occurred: " ⊕ botTerminationError
