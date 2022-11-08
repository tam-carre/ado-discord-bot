{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network (fetchJson, fetchJson') where

-- Downloaded libraries
import Data.Aeson (Value (..), FromJSON)
import Network.HTTP.Simple
  ( httpJSONEither
  , JSONException
  , Request
  , Response
  , getResponseBody
  , getResponseStatusCode
  )

-------------------------------------------------------------------------------

-- | DEPRECATED
fetchJson :: MonadIO m => Text -> (Value -> Either Text b) -> Request -> m (Either Text b)
fetchJson context parser request = do
  resp <- httpJSONEither request

  let json :: Either JSONException Value
      json   = getResponseBody resp
      status = getResponseStatusCode resp
      err    = pure . Left . (("[" <> context <> "] ") <>)

  case (status, json) of
    (200, Right value) -> pure $ parser value
    (200, _)           -> err "Failed to decode JSON"
    (code, resp')      -> err (show code <> ": " <> show resp')

fetchJson' :: forall a. forall m. (MonadIO m, FromJSON a) => Request -> m (Either Text a)
fetchJson' request = do
  resp <- (httpJSONEither request :: MonadIO m => m (Response (Either JSONException a)))

  let jsonEither :: Either JSONException a
      jsonEither = getResponseBody resp
      status     = getResponseStatusCode resp
      isOk       = status >= 200 && status <= 299

  case (isOk, jsonEither) of
    (True, Left exc) -> pure . Left $ "Failed to decode JSON: " <> show exc
    (True, Right v)  -> pure $ Right v
    _                -> pure . Left $ "Non-200 error code: " <> show status
