module Proj2 where
import Data.Char
import Data.List
import Text.Printf
import qualified Data.Set as Set

type Location  = (Int, Int)
type GameState = Set.Set [Location]

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just ((ord (toLower x) - ord 'a' + 1), (read xs :: Int))
toLocation _      = Nothing

mustToLocation :: String -> Location
mustToLocation x = case toLocation x of
                    Just y -> y
                    Nothing -> (-99, -99) :: Location

fromLocation :: Location -> String
fromLocation (x, y) = (printf "%c%d" (toUpper (chr (ord 'a' + x - 1))) y)

locationToInt :: Location -> Int
locationToInt (x, y) = (y * 8) - 8 + x

radius :: Location -> Int -> Set.Set Location
radius a y = Set.fromList [intToLocation(x) | x <- [1..8*4], (distance (intToLocation x) a) == y]

intToLocation :: Int -> Location
intToLocation x = ((((x-1) + 8 ) `rem` 8) + 1, ((( x -1 ) + 8) `div` 8))

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback x y = (distinctDistances x y 0, distinctDistances x y 1, distinctDistances x y 2)

distinctDistances :: (Integral t, Real a, Num p) => [(a, a)] -> [(a, a)] -> t -> p
distinctDistances a [] n = 0
distinctDistances a (b:bs) n =  sum [1 | minimum (allDistances b a) == n] + distinctDistances a bs n

allDistances :: (Integral a1, Real a2) => (a2, a2) -> [(a2, a2)] -> [a1]
allDistances a b = [distance c a | c <- b]

distance :: (Real a, Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

validLocations :: Location -> Location -> Set.Set [Location]
validLocations a b = Set.fromList (combinations [intToLocation y | y <- [locationToInt a .. locationToInt b]] 3)

initialGuess :: ([Location],GameState)
initialGuess = ([intToLocation 1, intToLocation 16, intToLocation 32],  validLocations (mustToLocation "A1") (mustToLocation "H4"))


nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (x, state) (3, 0, 0) = (x, state)
nextGuess ([x, y, z], state) (a, b, c) = (head (Set.toList foo), foo)
                                                                where foo = possibleLocations ([x, y, z], state) (a, b, c)

combinations xs n = filter ((n==).length.nub) (mapM (const xs) [1..n])

possibleLocations :: ([Location],GameState) -> (Int, Int, Int) -> Set.Set [Location]
possibleLocations ([x, y, z], state) (a, b, c) = Set.fromList [[q, w, e] | [q, w, e] <- Set.toList state, (feedback [q, w, e] [x, y, z]) ==  (a, b, c) ]
