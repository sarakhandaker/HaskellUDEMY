import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "How to write a test" $ do
    it "should be able to run tests" $ do
      someString `shouldBe` "someString"
      