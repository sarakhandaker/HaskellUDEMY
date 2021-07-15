import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word = 
    let (Just result) = findWord gwc word
        string = map cell2char result
    in string `shouldBe` word

main :: IO ()
main = hspec $ do
   describe "formatGrid" $ do
    it "should concatenate every line with a new line" $ do
     ( formatGrid (gridWithCoords ["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "should find words that exist in the grid" $ do
            testFindWord "HASKELL"
            testFindWord "PERL"
        it "should not find words that do not exist in the grid" $ do    
            findWord gwc "HAMSTER" `shouldBe` Nothing

    describe "findWords" $ do
        it "should find all the words that exist in the grid" $ do
            let found = findWords gwc languages 
                asString = map (map cell2char) found
            asString `shouldBe` languages
        it "should not find all the words that do not exist in the grid" $ do
            findWords gwc ["FRENCH", "GERMAN", "ENGLISH"] `shouldBe` []