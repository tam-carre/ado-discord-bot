{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module DeeplSpec (spec) where

-- Ado Bot modules
import Deepl.Internal (Tl (..), translateApi)
import Network        (fetchJson)

-- Downloaded libraries
import Test.Hspec (Spec, shouldBe, it)
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

spec :: Spec
spec = do
  it "Should succeed making a DeepL API call and parsing the response" $ do
    resp <- fetchJson @Tl (translateApi "Bonjour")
    isRight resp `shouldBe` True

  it "Should succeed parsing a valid payload" $ do
    let
      expectedResult = Right (Tl "EN" "Hallo, Welt!")
      validPayload = "{ \"translations\": [ { \"detected_source_language\": \"EN\", \"text\": \"Hallo, Welt!\" } ] }"

    eitherDecode @Tl validPayload `shouldBe` expectedResult
