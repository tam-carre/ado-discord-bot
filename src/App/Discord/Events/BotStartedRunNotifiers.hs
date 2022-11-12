--  Why this handler and not just the Ready event?
--
-- The Discord Gateway docs do not guarantee that Ready is only emitted once.
-- In case of connection issues we do not know whether Discord will re-emit
-- the Ready event. Therefore, tasks which should only be started once per
-- session should be initiated in this handler for discord-haskell's
-- `discordOnStart` property.
--
-- NOTE: Although this returns a `DiscordHandler ()` and hence has access
-- to Gateway requests, `discordOnStart` is actually run before receiving
-- any Ready event. The functionality to ensure tasks are run only once,
-- and only after Ready is fired, is not implemented.
--
-- NOTE2: This handler blocks bot execution until it returns.

{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module App.Discord.Events.BotStartedRunNotifiers (onBotStartedRunNotifiers) where

import App                                (App)
import App.Deepl                          (translate)
import App.Discord.Events.NewYTChatMsg    (onNewYTChatMsg)
import App.Discord.Events.Notify          (notify)
import App.Lenses                         (author, avatar, communityPostCh, communityPostRole,
                                           content, desc, embedContent, embedUrl, id, msgTxt,
                                           relayCh, secretBaseCh, secretBaseRole, settingsToCh,
                                           settingsToRole, thumb, title, url, view, (.~), (?~),
                                           (^.))
import App.Notifications.SecretBase       (getNextNewSecretBaseStream, getNextNewSecretBaseVid)
import App.Notifications.YTCommunityPosts (getNextNewCommunityPost)
import App.Notifications.YTLivestream     (getNextNewLivestream)
import App.Utils                          (forkIO_, sleep)
import Data.Default                       (Default (def))
import System.Process                     (shell)
import System.Process.Streaming           (execute, foldOutErr, piped)
import System.Process.Streaming.Text      qualified as PT

----------------------------------------------------------------------------------------------------

-- | Good to know: This handler blocks bot execution until it returns.
onBotStartedRunNotifiers ∷ App ()
onBotStartedRunNotifiers = do
  echo "Bot started."
  forkIO_ $ do
    sleep 5 -- This may help waiting till the bot is ready
    echo "Starting notifiers."
    watch getNextNewCommunityPost    onNewCommunityPost
    watch getNextNewSecretBaseStream onNewSecretBaseStream
    watch getNextNewSecretBaseVid    onNewSecretBaseVid
    watch getNextNewLivestream       onNewYTStream
  where
  watch watcher handler = forkIO_ $ do
    justCameOut ← watcher
    forkIO_ $ handler justCameOut
    sleep 30
    watch watcher handler

  onNewCommunityPost post = do
    notify $ def & settingsToCh   .~ view communityPostCh
                 & settingsToRole .~ view communityPostRole
                 & thumb          ?~ post^.avatar
                 & author         .~ post^.author
                 & embedContent   .~ post^.content
                 & embedUrl       .~ "https://youtube.com/post/" ⊕ post^.id
                 & msgTxt         ?~ "Ado has just published a community post! \n"
                                   ⊕ "<https://youtube.com/post/" ⊕ post^.id ⊕ ">"
    translate (post^.content) ≫= \case
      Left err → echo err
      Right tl → notify $ def & settingsToCh .~ view communityPostCh
                              & embedContent .~ "*[DeepL]* " ⊕ tl
                              & embedUrl     .~ "https://youtube.com/post/" ⊕ post^.id

  onNewSecretBaseStream live =
    notify $ def & settingsToCh   .~ view secretBaseCh
                 & settingsToRole .~ view secretBaseRole
                 & thumb          ?~ live^.thumb
                 & author         .~ live^.title
                 & embedContent   .~ live^.desc
                 & embedUrl       .~ live^.url
                 & msgTxt         ?~ "Ado is live on Secret Base!"

  onNewSecretBaseVid vid =
    notify $ def & settingsToCh   .~ view secretBaseCh
                 & settingsToRole .~ view secretBaseRole
                 & thumb          ?~ vid^.thumb
                 & author         .~ vid^.title
                 & embedContent   .~ vid^.desc
                 & embedUrl       .~ vid^.url
                 & msgTxt         ?~ "Ado has just uploaded a video on Secret Base!"

  onNewYTStream vidId = do
    notify $ def & settingsToCh .~ view relayCh
                 & embedUrl     .~ "https://youtu.be/" ⊕ vidId
                 & embedContent .~ "Ado is live at " ⊕ "https://youtu.be/" ⊕ vidId
                                ⊕ " ! I will relay live translations here."
    env ← ask
    discordHandle ← lift ask
    let onMsgDiscord = usingReaderT env . onNewYTChatMsg
        onMsgIO      = usingReaderT discordHandle . onMsgDiscord
    liftIO $ execute
      (piped . shell $ "node ./masterchat/index.js " ⊕ toString vidId)
      (foldOutErr . PT.bothAsUtf8x . PT.combinedLines . PT.eachLine $ onMsgIO)
