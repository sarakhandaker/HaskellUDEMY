import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
   describe "formatGrid" $ do
    it "should concatenate every line with a new line" $ do
      formatGrid ["abc", "def", "ghi"] `shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "should find words that exist in the grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "PERL" `shouldBe` Just "PERL"
        it "should not find words that do not exist in the grid" $ do    
            findWord grid "HAMSTER" `shouldBe` Nothing

    describe "findWords" $ do
        it "should find all the words that exist in the grid" $ do
            findWords grid languages `shouldBe` languages
        it "should not find all the words that do not exist in the grid" $ do
            findWords grid ["FRENCH", "GERMAN", "ENGLISH"] `shouldBe` []