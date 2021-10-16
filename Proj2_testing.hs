module Main where
import Test.QuickCheck
import Text.Printf
import Data.Char
import Proj2
import Test.Hspec
import Test.QuickCheck

checkLocation ::  Char -> Int -> Property
checkLocation x y =   elem x ['A' .. 'H']
         ==> ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
         === Just (printf "%c%d" x y)

prop_abs :: Int -> Int
prop_abs n = n

main :: IO ()
main = hspec $ do
  describe "Prelude.read" $ do
    it "can parse integers" $ do
      (toLocation "a2") `shouldBe` (1, 2)

