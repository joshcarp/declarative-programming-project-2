module Proj2 where
import Data.Char
import Data.List
import Text.Printf
import Data.Set

type Location  = (Int, Int)
type GameState =  Set [Location]

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just ((ord x - ord 'a' + 1), (read xs :: Int))
toLocation _      = Nothing

fromLocation :: Location -> String
fromLocation (x, y) = (printf "%c%d" (chr (ord 'a' + x - 1)) y)

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback x y = (distinctDistances x y 0, distinctDistances x y 1, distinctDistances x y 2)


allLocations ::  Set [Location]
allLocations =  fromList (combinations 3 [(x, y) | x <- [1..8], y <- [1..4]])

initialGuess :: ([Location],GameState)
initialGuess = ([(6,1),(8,3),(5,4)],  allLocations)

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (guess, state) results = (head (Data.Set.toList foo), foo) where foo = reduceLocations (guess, state) results


reduceLocations :: ([Location],GameState) -> (Int, Int, Int) ->  Set [Location]
reduceLocations (guess, state) result =  fromList [position | position <-  toList state, (feedback position guess) ==  result ]


-- List Functions --

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] , x <- combinations (n-1) (Data.List.drop (i+1) xs) ]


distance :: (Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

distinctDistances :: (Real a, Integral t, Num p) => [(a, a)] -> [(a, a)] -> t -> p
distinctDistances a [] n = 0
distinctDistances a (b:bs) n =  sum [1 | minimum ([distance c b | c <- a]) == n] + distinctDistances a bs n