module PlayfairCipherSpec (spec) where

import Lab_1.PlayfairCipher (decode, encode)

import Test.Hspec

spec :: Spec
spec = do
    describe "Lab_1.PlayfairCipher" $ do
        describe "encode" $ do
            it "returns `Nothing` for an empty string" $ do
                encode "" "" `shouldBe` Nothing

            it "encodes a string with spaces correctly" $ do
                encode "hide the gold in the tree stump" "playfair example" `shouldBe` Just "bmodzbxdnabekudmuixmmouvif"

            it "encodes a string with repeated characters correctly" $ do
                encode "balloon" "playfair example" `shouldBe` Just "dpyranqo"

            it "ignores non-alphabetic characters in the keyword" $ do
                encode "test" "playfair123" `shouldBe` Just "nmtn"

            it "encodes a string with mixed case correctly" $ do
                encode "Hide The Gold" "Playfair Example" `shouldBe` Just "bmodzbxdnage"

        describe "decode" $ do
            it "returns `Nothing` for an empty string" $ do
                decode "" "" `shouldBe` Nothing

            it "decodes a common encoded string correctly" $ do
                decode "bmodzbxdnabekudmuixmmouvif" "playfair example" `shouldBe` Just "hidethegoldinthetrexestump"

            it "decodes a string encoded with repeated characters correctly" $ do
                decode "dpyranqo" "playfair example" `shouldBe` Just "balxloon"

            it "ignores non-alphabetic characters in the keyword during decoding" $ do
                decode "bmodzbxd" "playfair123" `shouldBe` Just "dhtrwdzc"

            it "decodes a string with mixed case correctly" $ do
                decode "bmodzbxdnabekudmuixmmouvif" "Playfair Example" `shouldBe` Just "hidethegoldinthetrexestump"

            it "decodes a string with filler characters correctly" $ do
                decode "dpyranqo" "playfair example" `shouldBe` Just "balxloon"
