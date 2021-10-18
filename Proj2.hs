{- Code for Project 2, Semester 2, 2021
*    Author:         Joshua Carpeggiani
*    E-mail:         jcarpeggiani@student.unimelb.edu.au
*    Student ID:     999380
*    Subject Code:   COMP30020
*    Purpose:        Code for Project 2, Semester 2, 2021. Battleship Implementations.
*
*    This implementation takes a "generate and test" approach; every time feedback is recorded all the possible positions are calculated for that feedback with those choices. The game state is then set to the intersection of these possible locations and the existing game state.
*
-}


module Proj2 where
import Data.Char
import Data.List
import Text.Printf
import Data.Maybe

type Location  = (Int, Int)
type GameState =  [[Location]]


-- | toLocation returns a Maybe Location if the string is a valid Location and Nothing if it's not a valid Location.
--   Example
--   > toLocation "A4"
--   Just (65,4)
toLocation :: String -> Maybe Location
toLocation (x:xs) = Just (ord  x, read xs :: Int)
toLocation _      = Nothing

-- | fromLocation returns a string for a location
--   Example:
--   > fromLocation (65, 2)
--   "A2"
fromLocation :: Location -> String
fromLocation (x, y) = printf "%c%d" (chr x) y


-- | feedback returns a tuple for the distances that are zero, one and two units away from a target.
--   Example:
--   > feedback [(1, 2), (1, 3), (4, 5)] [(1, 2), (1, 3), (4, 6)]
--   (2,1,0)
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback x y = (minDistance x y 0, minDistance x y 1, minDistance x y 2)

-- | allLocations returns a list of all combinations of length 3
--   Example:
--   > allLocations
--   [[(65,1),(65,2),(65,3)],[(65,1),(65,2),(65,4)]...]
allLocations ::  [[Location]]
allLocations =  combinations 3 [(ord(x), y) | x <- ['A'..'H'], y <- [1..4]]

-- | initialGuess returns an initial guess and all of the valid choice combinations.
-- The location was calculated in Main2.hs by calculating every initial guess against a pool of random targets
-- After 6 hours of computation the location was chosen as ["F1", "G4", "D2"] with an average of 5.7 choices to get to an answer.
-- +---+---+---+---+---+---+---+---+---+---+
-- |   | A | B | C | D | E | F | H | G |   |
-- +---+---+---+---+---+---+---+---+---+---+
-- | 1 |   |   |   |   |   | X |   |   |   |
-- +---+---+---+---+---+---+---+---+---+---+
-- | 2 |   |   |   | X |   |   |   |   |   |
-- +---+---+---+---+---+---+---+---+---+---+
-- | 3 |   |   |   |   |   |   |   |   |   |
-- +---+---+---+---+---+---+---+---+---+---+
-- | 4 |   |   |   |   |   |   |   | X |   |
-- +---+---+---+---+---+---+---+---+---+---+
--   Example:
--   > initialGuess
--    ([(70,1),(71,4),(68,2)], [[(65,1),(65,2),(65,3)]..])
initialGuess :: ([Location],GameState)
initialGuess = ([fromJust (toLocation x) | x <- ["F1", "G4", "D2"]],  allLocations)

-- | nextGuess filters out the possible combinations in GameState by calculating all the possible places
--  that the three targets could be hiding and filtering the places that isn't in that list.
--   Example:
--   > nextGuess ([(1, 2), (3, 4), (5, 6)], [[(1, 2), (3, 4), (5, 6)]]) (3, 0, 0)
--     ([(1,2),(3,4),(5,6)],[[(1,2),(3,4),(5,6)]])
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (guess, state) results = (head newState, newState) where newState = [choice | choice <- state, feedback choice guess ==  results ]

-- List Functions --

-- | combinations returns the combinations of length n from the list xs.
--   Example:
--   > combinations 2 [1, 2, 3]
--   [[1,2],[1,3],[2,3]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] , x <- combinations (n-1) (drop (i+1) xs) ]


-- | distance calculates the euclidean distance between two tuples.
--   Example:
--   > distance (1, 2) (3, 1)
--   2
distance :: (Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

-- | minDistance returns the minimum distance between every tuple in the first list to all the points in the second list.
--   Example:
--   > minDistance [(1, 2)] [(4, 5), (1, 2)] 0
--   1
minDistance :: (Real a, Integral t, Num p) => [(a, a)] -> [(a, a)] -> t -> p
minDistance _ [] _ = 0
minDistance a (b:bs) n =  sum [1 | minimum ([distance c b | c <- a]) == n] + minDistance a bs n