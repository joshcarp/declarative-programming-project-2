module Main where
import Test.QuickCheck
import Text.Printf
import Data.Char
import Proj2

prop ::  Char -> Int -> Property
prop x y =  elem x ['a' .. 'z'] &&
            y >= 0 &&
            isAlphaNum(x) &&
            isAsciiLower(x)
            ==>
            ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
            ===
            Just (printf "%c%d" x y)


main :: IO ()
main = quickCheck (verbose prop)
