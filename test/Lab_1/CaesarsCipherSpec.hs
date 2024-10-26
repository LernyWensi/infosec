module CaesarsCipherSpec (spec) where

import Lab_1.CaesarsCipher (decode, encode)

import Test.Hspec

spec :: Spec
spec = do
    describe "Lab_1.CaesarsCipher" $ do
        describe "encode" $ do
            it "with positive offset" $ do
                encode 5 "Hello, World!" `shouldBe` "Mjqqt1%\\twqi&"

            it "with negative offsets" $ do
                encode (-3) "def" `shouldBe` "abc"

            it "empty strings" $ do
                encode 8 "" `shouldBe` ""

            it "with wrapping from start" $ do
                encode 1114111 "S" `shouldBe` "R"

            it "emojis" $ do
                encode 3 "Hello 👋" `shouldBe` "Khoor#👎"

        describe "decode" $ do
            it "with positive offset" $ do
                decode 5 "Mjqqt1%\\twqi&" `shouldBe` "Hello, World!"

            it "with negative offsets" $ do
                decode (-3) "abc" `shouldBe` "def"

            it "empty strings" $ do
                decode 8 "" `shouldBe` ""

            it "with wrapping from start" $ do
                decode 1114111 "R" `shouldBe` "S"

            it "emojis" $ do
                decode 3 "Khoor#👎" `shouldBe` "Hello 👋"
