{-# LANGUAGE OverloadedStrings #-}

module Json ((?.), (?!!), unObj, unArr, unStr, (.->)) where

-- Downloaded libraries
import Data.Vector      ((!?))
import Data.Aeson       (Value (..), Key, Object, Array, FromJSON, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM

-------------------------------------------------------------------------------

(.->) :: FromJSON a => Parser Object -> Key -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

-- | Safely accessing a JSON object property (Optional Chaining)
(?.) :: Either Text Value -> Key -> Either Text Value
(?.) objEither prop = objEither >>= unObj >>=
  \km -> case KM.lookup prop km of
    Just val -> Right val
    Nothing  -> Left $ "Expected to find  .\"" <> show prop <>
                       "but found nothing" <> " inside object " <> show km

-- | Safely accessing a JSON array element (JS equivalent: arr?.[index]?.)
(?!!) :: Either Text Value -> Int -> Either Text Value
(?!!) arrEither index = arrEither
  >>= unArr
  >>= \v -> case v !? index of
        Nothing  -> Left $ "Expected to find something but found nothing at index"
                        <> show index <> "of the following array: " <> show v
        Just val -> Right val

-- | Safely unwrapping a JSON value that you think is an Object into a KeyMap
unObj :: Value -> Either Text Object
unObj (Object obj) = Right obj
unObj other        = Left $ "Expected Object, got: " <> show other

-- | Safely unwrapping JSON val that you think is an Array into a Vector
unArr :: Value -> Either Text Array
unArr (Array arr) = Right arr
unArr other       = Left $ "Expected Array, got: " <> show other

-- | Safely unwrapping JSON val that you think is a String into a Text
unStr :: Value -> Either Text Text
unStr (String txt) = Right txt
unStr other        = Left $ "Expected String, got: " <> show other
