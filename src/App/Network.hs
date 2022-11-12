module App.Network (fetchJson) where

import Data.Aeson          (FromJSON)
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatusCode, httpJSONEither)

----------------------------------------------------------------------------------------------------

fetchJson ∷ (FromJSON a, MonadIO m) ⇒ Request → m (Either Text a)
fetchJson request = do
  resp ← httpJSONEither request
  let status = getResponseStatusCode resp

  case (status ∈ [200..299], getResponseBody resp) of
    (True, Right x) → pure $ Right x
    (True, Left e)  → pure . Left $ "Failed to decode JSON: " ⊕ show e
    _               → pure . Left $ "Non-2xx  error code: " ⊕ show status
