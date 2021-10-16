module Main where
import Test.QuickCheck
import Text.Printf
import Data.Char
import Proj2

prop :: Char -> Int -> Property
prop x y =
            y >= 0 &&
            isAlphaNum(x) &&
            isAsciiLower(x)
            ==>
            toLocation(printf "%x%d" x y) >>= (\r -> fromLocation r)
            === toLocation(printf "%x%d" x y) >>= (\r -> fromLocation r)


main :: IO ()
main = quickCheck (prop)
