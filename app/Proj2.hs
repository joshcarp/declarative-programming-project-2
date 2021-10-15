module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char

data Location = Location Int Int
type GameState = Int

toLocation :: String -> Maybe Location
toLocation (x:y:[]) = Just (Location (ord x - ord 'a') (digitToInt y))


fromLocation :: Location -> String
fromLocation _ = ""

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ _ = (0, 0, 0)

initialGuess :: ([Location],GameState)
initialGuess = ([Location 1 2],1)

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess _ _ = ([Location 1 2],1)


