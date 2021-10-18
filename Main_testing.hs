{-
*    Code for Project 2, Semester 2, 2021
*    Author:         Joshua Carpeggiani
*    E-mail:         jcarpeggiani@student.unimelb.edu.au
*    Student ID:     999380
*    Subject Code:   COMP30020
*    Purpose:        Optimisation code for Project 2
*
*    This file was used for testing the initial starting guess and
*    optimising it to an initial guess that would decrease
*    the guesses required overall.
*    It was adapted from Main.hs written by Peter Schachte.
-}
module Main where

import Data.List
import Proj2
import System.Exit
import System.IO.Unsafe
import System.Random
import Text.Printf

average xs = realToFrac (sum xs) / genericLength xs

testCase = "C3 F1 F3"

-- | Main code to test Proj2 implementations within Grok. This will be run with
-- no command line arguments, so there's no way to specify the target to search
-- for. Therefore, I've hardwired one test, but students will need to do further
-- testing on their own.
main :: IO ()
main = do
  case mapM toLocation $ words testCase of
    Just target@[_, _, _] -> proj2test target
    _ -> do
      putStrLn $
        "toLocation Failed to convert one of " ++ testCase ++ " to a Location"
      exitFailure

-- | Guess the given target, counting and showing the guesses.
proj2test :: [Location] -> IO ()
proj2test target = do
  putStrLn $ "Searching for target " ++ showLocations target
  let (guess, other) = initialGuess
  loop target guess other 1

-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Location] -> [Location] -> Proj2.GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3, 0, 0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess', other') = nextGuess (guess, other) answer
      loop target guess' other' (guesses + 1)

showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)

-- | loop2Start prints out how many choices it took to solve a
--   puzzle on average with a starting position.
loop2Start :: [Location] -> IO ()
loop2Start target = do
  putStrLn $ "Searching for target " ++ showLocations target
  let initial = target
  let a =
        average $
        take 50 $ map (findChoicesForGame initial allLocations 1) $ allLocations
  printf "%s\n" (show a)

-- | allLocations2 maps applies findAverageChoices to allLocations2
allLocations2 = map findAverageChoices allLocations

-- | findAverageChoices returns the average number of
--   choices to win a match for a given starting location.
findAverageChoices :: Fractional a => [Location] -> ([Location], a)
findAverageChoices initial =
  ( initial
  , (average
       (take 10 (map (findChoicesForGame initial allLocations 1) allLocations))))

-- | findChoicesForGame takes a starting guess and a state and returns the
--   number of guesses it took to win the game.
findChoicesForGame :: [Location] -> GameState -> Int -> [Location] -> Int
findChoicesForGame guess state guesses target
  | answer == (3, 0, 0) = guesses
  | otherwise = findChoicesForGame guess2 state2 (guesses + 1) target
  where
    answer = feedback target guess
    (guess2, state2) = nextGuess (guess, state) answer
