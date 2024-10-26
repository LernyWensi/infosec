import Test.Hspec

import CaesarsCipherSpec
import PlayfairCipherSpec

main :: IO ()
main = hspec $ do
    CaesarsCipherSpec.spec
    PlayfairCipherSpec.spec
