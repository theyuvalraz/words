import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            (formatGrid ["abc","def","ghi"]) `shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
            findWord grid "HASKELL" `shouldBe` True
            findWord grid "PERL" `shouldBe` True
        it "Should not find words that do not exist on the Grid" $ do
            findWord grid "YUVAL" `shouldBe` False

    