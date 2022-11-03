{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE LambdaCase         #-}

module Notifications.History
  ( getNotifHistoryDb
  , getNotifHistory
  , changeNotifHistory
  , NotifHistoryDb (..)
  ) where

-- Ado Bot modules
import Utils (tap)

-- Downloaded libraries
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Acid
  ( Update
  , Query
  , makeAcidic
  , openLocalState
  , update
  , query
  , AcidState
  )

-- Base
import Control.Exception (handle)

-------------------------------------------------------------------------------

data NotifHistoryDb = NotifHistoryDb
  { community  :: [Text]
  , secretBase :: [Text]
  , ytStream   :: [Text]
  }

$(deriveSafeCopy 0 'base ''NotifHistoryDb)

-------------------------------------------------------------------------------

-- Internals (Due to TH they cannot be at the bottom)

getHistory :: Query NotifHistoryDb NotifHistoryDb
getHistory = ask

upsert :: NotifHistoryDb -> Update NotifHistoryDb ()
upsert = put

$(makeAcidic ''NotifHistoryDb ['getHistory, 'upsert])

------------------------------------------------------------------------------

-- Exported bindings

-- | Get the notif history DB. Should be done once per runtime.
getNotifHistoryDb :: IO (AcidState NotifHistoryDb)
getNotifHistoryDb = openLocalState (NotifHistoryDb [] [] [])
  & tap (\_ -> echo "Successfully connected to the notification history DB")
  & handle (\e -> die $ "Problem accessing notification history: " <> show (e :: SomeException))

-- | Get the bot's notif history
getNotifHistory :: MonadIO m => AcidState NotifHistoryDb -> m NotifHistoryDb
getNotifHistory db = liftIO $ query db GetHistory

-- | Apply a function over the bot's notif history
changeNotifHistory :: MonadIO m
  => AcidState NotifHistoryDb
  -> (NotifHistoryDb -> NotifHistoryDb)
  -> m ()
changeNotifHistory db f = liftIO $ do
  old <- getNotifHistory db
  update db $ Upsert (f old)
