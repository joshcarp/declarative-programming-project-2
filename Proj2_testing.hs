module Main where
import Test.QuickCheck
import Text.Printf
import Data.Char
import Proj2
import Test.Hspec
import Test.QuickCheck


prop_abs :: Int -> Int
prop_abs n = n

main :: IO ()
main = testCheckLocation >>
       testLocation

-- QuickCheck Tests
testCheckLocation = quickCheck checkLocation

checkLocation ::  Char -> Int -> Property
checkLocation x y =   elem x ['A' .. 'H']
         ==> ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
         === Just (printf "%c%d" x y)

-- Unit Tests
testLocation = hspec $ do
          describe "Prelude.read" $ do
            it "can parse integers" $ do
              (toLocation "a2") `shouldBe` Just ((1, 2) :: Location)
