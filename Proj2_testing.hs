module Main where
import Test.QuickCheck
import Text.Printf
import Data.Char
import Proj2

checkLocation ::  Char -> Int -> Property
checkLocation x y =   elem x ['A' .. 'H']
         ==> ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
         === Just (printf "%c%d" x y)

main :: IO ()
main = quickCheck (checkLocation)
