module Test where
import Proj2
import Test.QuickCheck
import Text.Printf
import Data.Char
import Test.Hspec
import Test.QuickCheck


prop_abs :: Int -> Int
prop_abs n = n

main :: IO ()
main = testCheckLocation >>
       testToLocation >>
       testDistance

-- QuickCheck Tests
testCheckLocation = quickCheck checkLocation

checkLocation ::  Char -> Int -> Property
checkLocation x y =   elem x ['A' .. 'H']
         ==> ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
         === Just (printf "%c%d" x y)

-- Unit Tests
testToLocation = hspec $ do
          describe "testToLocation" $ do
            it "toLocation" $ do
              (toLocation "a2") `shouldBe` Just ((1, 2) :: Location)

{-
    1  2  3  4  5
-------------------
1 | 2  2  2  2  2
2 | 2  1  1  1  2
3 | 2  1  0  1  2
4 | 2  1  1  1  2
5 | 2  2  2  2  2

-}
testDistance = hspec $ do
          describe "Prelude.read" $ do
            it "can parse integers" $ do
              distance (3, 3) (3, 3) `shouldBe` 0
