module Proj2 (Location, toLocation, fromLocation, feedback, GameState, initialGuess, nextGuess) where

import Data.Char
import Text.Printf
data Location = Location Int Int deriving (Eq, Show)
type GameState = Int

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just (Location (ord x - ord 'A') (read xs :: Int))
toLocation _ = Nothing

fromLocation :: Location -> String
fromLocation (Location x y) = (printf "%c%d" (chr (ord 'A' + x)) y)

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ _ = (0, 0, 0)

initialGuess :: ([Location],GameState)
initialGuess = ([Location 1 2],1)

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess _ _ = ([Location 1 2],1)


