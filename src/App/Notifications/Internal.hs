{-# LANGUAGE LambdaCase, RankNTypes #-}

module App.Notifications.Internal (addToHistory, returnWhenFound) where

import App                       (App)
import App.Notifications.History (NotifHistoryDb, changeNotifHistory)
import App.Utils                 (sleep)
import Control.Lens              (Lens')

----------------------------------------------------------------------------------------------------

returnWhenFound ∷ App (Either Text a) → Text → App a
returnWhenFound fetcher successLogMsg = fetcher ≫= \case
  Right liveData → liveData <$ echo successLogMsg
  Left err → do
    echo err
    sleep 30
    returnWhenFound fetcher successLogMsg

{- HLINT ignore addToHistory -}
addToHistory ∷ Lens' NotifHistoryDb [a] → a → App ()
addToHistory lens entry = changeNotifHistory lens $ \hist → entry : take 50 hist
