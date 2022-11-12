{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DeriveDataTypeable, LambdaCase, TemplateHaskell, TypeFamilies #-}

module App.Notifications.History.Internal where

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

----------------------------------------------------------------------------------------------------

data NotifHistoryDb
  = NotifHistoryDb
    { _community ∷ [Text]
    , _sbStream  ∷ [Text]
    , _ytStream  ∷ [Text]
    , _sbVid     ∷ [Text]
    }

deriveSafeCopy 0 'base ''NotifHistoryDb

getHistory ∷ Query NotifHistoryDb NotifHistoryDb
getHistory = ask

upsert ∷ NotifHistoryDb → Update NotifHistoryDb ()
upsert = put

makeAcidic ''NotifHistoryDb ['getHistory, 'upsert]
