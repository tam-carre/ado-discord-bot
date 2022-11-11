{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module DiscordBot.Events.NewYTChatMsg.Internal
  ( Msg (..)
  , isAdo
  , adoMsg
  , tlMsg
  , isTl
  ) where

-- Downloaded libraries
import Control.Lens (makeFieldsNoPrefix, (^.))
import Data.Aeson (withObject, (.:), FromJSON (..))
import qualified Data.Text               as T

-------------------------------------------------------------------------------

data Msg = Msg { _content :: Text, _name :: Text, _chId :: Text }

makeFieldsNoPrefix ''Msg

instance FromJSON Msg where
  parseJSON = withObject "masterchat msg" $ \msg ->
    Msg <$> msg .: "content"
        <*> msg .: "authorName"
        <*> msg .: "authorChannelId"

isAdo :: Msg -> Bool
isAdo msg = msg^.chId == "UCln9P4Qm3-EAY4aiEPmRwEA"

adoMsg :: Msg -> Maybe Text -> Text
adoMsg msg tl =
  "<:AdoHappy:885833189086593024> **" <> (msg^.name) <> "**: " <> (msg^.content)
   <> maybe "" (\t -> "\n*[DeepL] " <> t <> "*") tl

tlMsg :: Text -> Text -> Text
tlMsg author' txt =
  ":speech_balloon: ||" <> author' <> ":|| `" <> txt <> "`"

isTl :: Text -> Bool
isTl = hasTlPrefix . T.stripStart . T.toLower where
  hasTlPrefix msg = any (`T.isPrefixOf` msg) tlPrefixes
  tlPrefixes      = ["[en", "tl:", "[tl", "en:", "eng:"]
