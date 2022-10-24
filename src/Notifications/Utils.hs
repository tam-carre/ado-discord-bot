{-# LANGUAGE LambdaCase #-}

module Notifications.Utils (returnWhenFound) where

-- Ado Bot modules
import Utils                 (sleep)
import Notifications.History (NotifHistoryDb)

-- Downloaded libraries
import Data.Acid  (AcidState)

-------------------------------------------------------------------------------

returnWhenFound :: MonadIO m
  => (AcidState NotifHistoryDb -> m (Either Text a))
  -> Text
  -> AcidState NotifHistoryDb
  -> m a
returnWhenFound fetcher successLogMsg db = fetcher db >>= \case
  Right liveData -> echo successLogMsg >> pure liveData
  Left err -> do
    echo err
    sleep 30
    returnWhenFound fetcher successLogMsg db
