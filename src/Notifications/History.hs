{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Notifications.History
  ( getNotifHistoryDb
  , getNotifHistory
  , changeNotifHistory
  , NotifHistoryDb (..)
  ) where

-- Ado Bot modules
import App   (App, Env (..))
import Utils (tap)
import Notifications.History.Internal (NotifHistoryDb (..), GetHistory (..), Upsert (..))

-- Downloaded libraries
import Data.Acid (openLocalState, update, query, AcidState)

-- Base
import Control.Exception (handle)

-------------------------------------------------------------------------------

-- Exported bindings

-- | Get the notif history DB. Should be done once per runtime.
getNotifHistoryDb :: IO (AcidState NotifHistoryDb)
getNotifHistoryDb = openLocalState (NotifHistoryDb [] [] [] [])
  & tap (\_ -> echo "Successfully connected to the notification history DB")
  & handle (\e -> die $ "Problem accessing notification history: " <> show (e :: SomeException))

-- | Get the bot's notif history
getNotifHistory :: App NotifHistoryDb
getNotifHistory = do
  db <- asks notifDb
  liftIO $ query db GetHistory

-- | Apply a function over the bot's notif history
changeNotifHistory :: (NotifHistoryDb -> NotifHistoryDb) -> App ()
changeNotifHistory f = do
  db <- asks notifDb
  old <- getNotifHistory
  liftIO . update db $ Upsert (f old)
