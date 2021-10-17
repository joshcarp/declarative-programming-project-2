module Proj2 where
import Data.Char
import Text.Printf
import qualified Data.Set as Set

type Location  = (Int, Int)
type GameState = Int

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just ((ord (toLower x) - ord 'a' + 1), (read xs :: Int))
toLocation _      = Nothing

fromLocation :: Location -> String
fromLocation (x, y) = (printf "%c%d" (toUpper (chr (ord 'a' + x - 1))) y)

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback x y = (distinctDistances x y 0, distinctDistances x y 1, distinctDistances x y 2)

distinctDistances :: (Integral t, Real a, Num p) => [(a, a)] -> [(a, a)] -> t -> p
distinctDistances a [] n = 0
distinctDistances a (b:bs) n =  sum [1 | minimum (allDistances b a) == n] + distinctDistances a bs n

allDistances :: (Integral a1, Real a2) => (a2, a2) -> [(a2, a2)] -> [a1]
allDistances a b = [distance c a | c <- b]

distance :: (Real a, Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

initialGuess :: ([Location],GameState)
initialGuess = ([(1, 1)], 1)

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess _ _ = ([(1, 1)], 1)


