import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import OA.Utils.Operators
import OA.Utils.RandState



main :: IO ()
main = hspec $ do
    describe "bitFlip" $ do
        it "[0,0] --> [0,1]" $
            bitFlip [0,0] 1 `shouldBe` [0,1]
        it "[0,0] --> [1,0]" $
            bitFlip [0,0] 0 `shouldBe` [1,0]

