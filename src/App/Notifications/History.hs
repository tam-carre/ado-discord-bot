{-# LANGUAGE RankNTypes, TypeFamilies #-}

module App.Notifications.History
  ( NotifHistoryDb (..)
  , changeNotifHistory
  , getNotifHistory
  , getNotifHistoryDb
  ) where

import App                                (App, Env (..))
import App.Notifications.History.Internal (GetHistory (..), NotifHistoryDb (..), Upsert (..))
import App.Utils                          (tap)
import Control.Exception                  (handle)
import Control.Lens                       (Lens', (%~))
import Data.Acid                          (AcidState, openLocalState, query, update)

----------------------------------------------------------------------------------------------------

-- | Get the notif history DB. Should be done once per runtime.
getNotifHistoryDb ∷ IO (AcidState NotifHistoryDb)
getNotifHistoryDb = openLocalState (NotifHistoryDb [] [] [] [])
  & tap (\_ → echo "Successfully connected to the notification history DB")
  & handle (\e → die $ "Problem accessing notification history: " ⊕ show (e ∷ SomeException))

-- | Get the bot's notif history
getNotifHistory ∷ App NotifHistoryDb
getNotifHistory = do
  db ← asks notifDb
  liftIO $ query db GetHistory

-- | Apply a function over the bot's notif history
changeNotifHistory ∷ Lens' NotifHistoryDb a → (a → a) → App ()
changeNotifHistory lens f = do
  db ← asks notifDb
  old ← getNotifHistory
  liftIO . update db $ Upsert (old & lens %~ f)
