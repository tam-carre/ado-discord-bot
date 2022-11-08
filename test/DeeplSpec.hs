{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module DeeplSpec (spec) where

-- Ado Bot modules
import Deepl.Internal (Tl (..))

-- Downloaded libraries
import Test.Hspec (Spec, shouldBe, it)
import Data.Aeson (eitherDecode)

-------------------------------------------------------------------------------

spec :: Spec
spec =
  it "Should succeed parsing a valid payload" $ do
    let
      expectedResult = Right (Tl "EN" "Hallo, Welt!")
      validPayload = "{ \"translations\": [ { \"detected_source_language\": \"EN\", \"text\": \"Hallo, Welt!\" } ] }"

    eitherDecode @Tl validPayload `shouldBe` expectedResult
