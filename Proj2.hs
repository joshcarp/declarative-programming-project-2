module Proj2 where
import Data.Char
import Text.Printf
import qualified Data.Set as Set

type Location  = (Int, Int)
type GameState = Set.Set Location

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just ((ord (toLower x) - ord 'a' + 1), (read xs :: Int))
toLocation _      = Nothing

mustToLocation :: String -> Location
mustToLocation x = case toLocation x of
                    Just y -> y
                    Nothing -> (-99, -99) :: Location


fromLocation :: Location -> String
fromLocation (x, y) = (printf "%c%d" (toUpper (chr (ord 'a' + x - 1))) y)
--
up :: Location -> Location
up (x, y) = (x, y - 1)

down :: Location -> Location
down (x, y) = (x, y + 1)

left :: Location -> Location
left (x, y) = (x - 1, y)

right :: Location -> Location
right (x, y) = (x + 1, y)

add :: Location -> Location
add x = intToLocation ((locationToInt x) + 1)

locationToInt :: Location -> Int
locationToInt (x, y) = (y * 8) - 8 + x

radius :: Location -> Int -> Set.Set Location
radius a y = Set.fromList [intToLocation(x) | x <- [1..8*4], (distance (intToLocation x) a) == y]

{-
X X X
X X X
X X X
-}

intToLocation :: Int -> Location
intToLocation x = ((((x-1) + 8 ) `rem` 8) + 1, ((( x -1 ) + 8) `div` 8))
--
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback x y = (distinctDistances x y 0, distinctDistances x y 1, distinctDistances x y 2)

distinctDistances :: (Integral t, Real a, Num p) => [(a, a)] -> [(a, a)] -> t -> p
distinctDistances a [] n = 0
distinctDistances a (b:bs) n =  sum [1 | minimum (allDistances b a) == n] + distinctDistances a bs n

allDistances :: (Integral a1, Real a2) => (a2, a2) -> [(a2, a2)] -> [a1]
allDistances a b = [distance c a | c <- b]

distance :: (Real a, Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

validLocations :: Location -> Location -> Set.Set Location
validLocations a b = Set.fromList [intToLocation y | y <- [locationToInt a .. locationToInt b]]


initialGuess :: ([Location],GameState)
initialGuess = ([(1, 1), (1, 2), (1, 3)], validLocations (mustToLocation "A1") (mustToLocation "H4"))

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (x, state) (3, 0, 0) = (x, state)
nextGuess ([x, y, z], state) (0, _, _) = ([x, y, z], (Set.difference state (Set.fromList [x, y, z])))
nextGuess ([x, y, z],  state) (zero, one, two) = ([], state)


{-


-}