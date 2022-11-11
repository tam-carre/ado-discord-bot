{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE LambdaCase         #-}

module Notifications.History.Internal where

-- Downloaded libraries
import Data.Acid     (Update, Query, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)

-------------------------------------------------------------------------------

data NotifHistoryDb = NotifHistoryDb
  { _community :: [Text]
  , _sbStream  :: [Text]
  , _ytStream  :: [Text]
  , _sbVid     :: [Text]
  }

deriveSafeCopy 0 'base ''NotifHistoryDb

getHistory :: Query NotifHistoryDb NotifHistoryDb
getHistory = ask

upsert :: NotifHistoryDb -> Update NotifHistoryDb ()
upsert = put

makeAcidic ''NotifHistoryDb ['getHistory, 'upsert]
