--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for proj2 project to be used in Grok


module Main where

import System.Exit
import Proj2
import Text.Printf
import System.Random
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.List

average xs = realToFrac (sum xs) / genericLength xs

testCase = "C3 F1 F3"

-- | Main code to test Proj2 implementations within Grok. This will be run with
-- no command line arguments, so there's no way to specify the target to search
-- for. Therefore, I've hardwired one test, but students will need to do further
-- testing on their own.
main :: IO ()
main = do
  case mapM toLocation $ words testCase of
    Just target@[_,_,_] ->
      proj2test target
    _ -> do
      putStrLn $ "toLocation Failed to convert one of " ++ testCase
                 ++ " to a Location"
      exitFailure


-- | Guess the given target, counting and showing the guesses.
proj2test :: [Location] -> IO ()
proj2test target = do
  putStrLn $ "Searching for target " ++ showLocations target
  let (guess,other) = initialGuess
  loop target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Location] -> [Location] -> Proj2.GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3,0,0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess',other') = nextGuess (guess,other) answer
      loop target guess' other' (guesses+1)

showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)
--
--loop2 :: [Location] -> [Location] -> Proj2.GameState -> Int -> Int
--loop2 target guess state guesses =
--    let answer = feedback target guess
--    if answer == (3,0,0)
--        then guesses
--        else loop2 target guess2 state2 (guesses+1)

loop2Start :: [Location] -> IO ()
loop2Start target = do
  putStrLn $ "Searching for target " ++ showLocations target
--  let l = loop2 guess other 1 target
  let other = validLocations
  let initial = target
  let a = average (take 10 (every 7 (map (loop2 initial other 1) (unsafePerformIO (shuffle (Set.toList other))))))
  printf "%s\n" (show a)

allLocations = map (loop2Start2) (unsafePerformIO (shuffle (Set.toList (validLocations ))))



loop2Start2 :: Fractional a => [Location] -> ([Location], a)
loop2Start2 initial = (initial, (average (take 10 (every 7 (map (loop2 initial other 1) (unsafePerformIO (shuffle (Set.toList other))))))))
                     where
                       other = validLocations


--average :: [Int] -> Real
--average xs = (realToFrac (sum xs) ) `div` (realToFrac (length xs))


every n xs = case drop (n-1) xs of
              y : ys -> y : every n ys
              [] -> []

loop2 :: [Location] -> GameState -> Int -> [Location] -> Int
loop2 guess state guesses target
                                | answer == (3,0,0) = guesses
                                | otherwise  = loop2 guess2 state2 (guesses+1) target
                                where
                                answer = feedback target guess
                                (guess2, state2) = nextGuess (guess, state) answer

shuffle :: (Eq a) => [a] -> IO [a]
shuffle [] = return []
shuffle ls = do
    x <- pick ls
    let y = remove x ls
    xs <- shuffle y
    return (x:xs)

remove :: (Eq a) => a -> [a] -> [a]
remove _ []     = []
remove r (x:xs) = if x == r then xs else x : remove r xs

pick :: [a] -> IO a
pick xs = do
    n <- randomRIO (0, length xs - 1)
    return $ xs !! n