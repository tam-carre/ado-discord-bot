{-# LANGUAGE TypeApplications #-}

module DeeplSpec (spec) where

import App.Deepl          (translate)
import App.Deepl.Internal (Tl (..), translateApi)
import App.Network        (fetchJson)
import Data.Aeson         (eitherDecode)
import Test.Hspec         (Spec, it, shouldBe)

----------------------------------------------------------------------------------------------------

spec ∷ Spec
spec = do
  it "Should succeed making a DeepL API call and parsing the response" $ do
    resp ← fetchJson @Tl (translateApi "Bonjour")
    isRight resp `shouldBe` True

  it "Should return a Left if the input is English" $ do
    tl ← translate "Hello"
    isLeft tl `shouldBe` True

  it "Should return a Right with the translation if the input is Japanese" $ do
    tl ← translate "愛してる"
    print tl
    isRight tl `shouldBe` True
    tl `shouldBe` Right "I love you"

  it "Should succeed parsing a valid payload" $ do
    let
      expectedResult = Right (Tl "EN" "Hallo, Welt!")
      validPayload = "{ \"translations\": [ { \"detected_source_language\": \"EN\", \"text\": \"Hallo, Welt!\" } ] }"

    eitherDecode @Tl validPayload `shouldBe` expectedResult
