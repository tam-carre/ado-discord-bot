module Json ((?.), (?!!), unObj, unArr, unStr) where

-- Downloaded libraries
import Data.Vector ((!?))
import Data.Aeson  (Value (..), Key, Object, Array)

import qualified Data.Aeson.KeyMap as KeyMap

-------------------------------------------------------------------------------

-- | Safely accessing a JSON object property (Optional Chaining)
(?.) :: Maybe Value -> Key -> Maybe Value
(?.) objMaybe prop = objMaybe >>= unObj >>= KeyMap.lookup prop

-- | Safely accessing a JSON array element (JS equivalent: arr?.[index]?.)
(?!!) :: Maybe Value -> Int -> Maybe Value
(?!!) arrMaybe index = arrMaybe >>= unArr >>= \v -> v !? index

-- | Safely unwrapping a JSON value that you think is an Object into a KeyMap
unObj :: Value -> Maybe Object
unObj (Object obj) = Just obj
unObj _            = Nothing

-- | Safely unwrapping JSON val that you think is an Array into a Vector
unArr :: Value -> Maybe Array
unArr (Array arr) = Just arr
unArr _           = Nothing

-- | Safely unwrapping JSON val that you think is a String into a Text
unStr :: Value -> Maybe Text
unStr (String txt) = Just txt
unStr _            = Nothing
