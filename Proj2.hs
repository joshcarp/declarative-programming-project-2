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
feedback x y = ((distancexx x y 0), (distancexx x y 1), (distancexx x y 2))

distancexx :: (Real a1, Integral a2) => [(a1, a1)] -> [(a1, a1)] -> a2 -> Int
distancexx a b z = sum [distancex a c z | c <- b]


foobar :: (Num a1, Num a2, Num a1) => [(a1, a1)] -> (a1, a1) -> a2 -> Int
foobar a b n = 2

distancex :: (Real a1, Integral a2) => [(a1, a1)] -> (a1, a1) -> a2 -> Int
distancex a b n = length [c | c <- a, (distance c b) == n ]

distance :: (Real a, Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

{-

[]
(0,0)
0	1	2
1	1	2
2	2	2 (2, 2)
-}

initialGuess :: ([Location],GameState)
initialGuess = ([(1, 1)], 1)

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess _ _ = ([(1, 1)], 1)


