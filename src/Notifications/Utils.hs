{-# LANGUAGE LambdaCase #-}

module Notifications.Utils (returnWhenFound) where

-- Ado Bot modules
import App   (App)
import Utils (sleep)

-------------------------------------------------------------------------------

returnWhenFound :: App (Either Text a) -> Text -> App a
returnWhenFound fetcher successLogMsg = fetcher >>= \case
  Right liveData -> echo successLogMsg >> pure liveData
  Left err -> do
    echo err
    sleep 30
    returnWhenFound fetcher successLogMsg
