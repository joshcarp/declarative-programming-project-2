module Proj2 where
import Data.Char
import Text.Printf
import qualified Data.Set as Set

type Location  = (Int, Int)
data GameState = GameState {
                           valid :: Set.Set Location,
                           lastGuess :: [Location],
                           lastFeedback :: (Int, Int, Int)
                           }

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just ((ord (toLower x) - ord 'a' + 1), (read xs :: Int))
toLocation _      = Nothing

mustToLocation :: String -> Location
mustToLocation x = case toLocation x of
                    Just y -> y
                    Nothing -> (-99, -99) :: Location

fromLocation :: Location -> String
fromLocation (x, y) = (printf "%c%d" (toUpper (chr (ord 'a' + x - 1))) y)

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

validLocations :: Location -> Location -> Set.Set Location
validLocations a b = Set.fromList [intToLocation y | y <- [locationToInt a .. locationToInt b]]

initialGuess :: ([Location],GameState)
initialGuess = ([(1, 1), (1, 2), (1, 3)],  GameState{
                                                     valid = validLocations (mustToLocation "A1") (mustToLocation "H4"),
                                                     lastGuess = [],
                                                     lastFeedback = (-1, -1, -1)
                                                     })


nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (x, state) (3, 0, 0) = (x, state)
nextGuess ([x, y, z], state) (0, b, c) = ([x, y, z], GameState{
                                                                valid = Set.difference (valid state) (Set.fromList [x, y, z]),
                                                                lastGuess = [x, y, z],
                                                                lastFeedback = (0, b, c)
                                                                })


encorporateFeedback :: ([Location],GameState) -> (Int,Int,Int) -> GameState
encorporateFeedback (locs, state) (0, 0, 0) = GameState{
                                                           valid = Set.difference (Set.difference (Set.difference (valid state) (radii locs 0)) (radii locs 1)) (radii locs 2),
                                                           lastGuess = locs,
                                                           lastFeedback = (0, 0, 0)
                                                           }

radii :: [Location] -> Int -> Set.Set Location
radii [] y = Set.empty
radii (x:xs) y = Set.union (radius x y) (radii xs y)


--findLocation :: ([Location],GameState) -> (Int, Int, Int) ->
