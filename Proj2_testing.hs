{-
*    Code for Project 2, Semester 2, 2021
*    Author:         Joshua Carpeggiani
*    E-mail:         jcarpeggiani@student.unimelb.edu.au
*    Student ID:     999380
*    Subject Code:   COMP30020
*    Purpose:        Tests
*
*    Battleship Testing Code
*    This file contains tests for Battleships,
*    and uses both unit tests and property based fuzz testing.
*
-}
module Test where

import Data.Char
import Proj2
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck
import Text.Printf

main :: IO ()
main = testCheckLocation >> testDistance >> testFeedback

-- | testCheckLocation runs quickCheck on checkLocation
testCheckLocation = quickCheck (withMaxSuccess 100 checkLocation)

-- | checkLocation Checks that any char and int can be converted
--   toLocation and fromLocation correctly.
checkLocation :: Char -> Int -> Property
checkLocation x y =
  elem x ['A' .. 'H'] ==>
  ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r))) ===
  Just (printf "%c%d" x y)

-- | toLoc converts a string to a Location and (-99, -99) if it fails.
toLoc x =
  case toLocation x of
    Just y -> y
    Nothing -> (-99, -99) :: Location

-- | testFeedback checks that the feedback location is valid with the testcases
--   that were provided in the project specification.
testFeedback =
  hspec $ do
    describe "testFeedback" $ do
      it "feedback" $ do
        feedback
          [(toLoc "H1"), (toLoc "B2"), (toLoc "D3")]
          [(toLoc "B3"), (toLoc "C3"), (toLoc "H3")] `shouldBe`
          (0, 2, 1)
      it "feedback" $ do
        feedback
          [(toLoc "H1"), (toLoc "B2"), (toLoc "D3")]
          [(toLoc "B1"), (toLoc "A2"), (toLoc "H3")] `shouldBe`
          (0, 2, 1)
      it "feedback" $ do
        feedback
          [(toLoc "H1"), (toLoc "B2"), (toLoc "D3")]
          [(toLoc "B2"), (toLoc "H2"), (toLoc "H1")] `shouldBe`
          (2, 1, 0)
      it "feedback" $ do
        feedback
          [(toLoc "A1"), (toLoc "D2"), (toLoc "B3")]
          [(toLoc "A3"), (toLoc "D2"), (toLoc "H1")] `shouldBe`
          (1, 1, 0)
      it "feedback" $ do
        feedback
          [(toLoc "A1"), (toLoc "D2"), (toLoc "B3")]
          [(toLoc "H4"), (toLoc "G3"), (toLoc "H2")] `shouldBe`
          (0, 0, 0)
      it "feedback" $ do
        feedback
          [(toLoc "A1"), (toLoc "D2"), (toLoc "B3")]
          [(toLoc "D2"), (toLoc "B3"), (toLoc "A1")] `shouldBe`
          (3, 0, 0)

{-

-- | testDistance uses the examples in the project specification
--   as unit tests for the distance function.

Grid for distance tests
    1  2  3  4  5
-------------------
1 | 2  2  2  2  2
2 | 2  1  1  1  2
3 | 2  1  0  1  2
4 | 2  1  1  1  2
5 | 2  2  2  2  2
-}
testDistance =
  hspec $ do
    describe "testDistance" $ do
      it "(3,3)(1,1)==2" $ do distance (3, 3) (1, 1) `shouldBe` 2
      it "(3,3)(1,2)==2" $ do distance (3, 3) (1, 2) `shouldBe` 2
      it "(3,3)(1,3)==2" $ do distance (3, 3) (1, 3) `shouldBe` 2
      it "(3,3)(1,4)==2" $ do distance (3, 3) (1, 4) `shouldBe` 2
      it "(3,3)(1,5)==2" $ do distance (3, 3) (1, 5) `shouldBe` 2
      it "(3,3)(2,1)==2" $ do distance (3, 3) (2, 1) `shouldBe` 2
      it "(3,3)(2,2)==1" $ do distance (3, 3) (2, 2) `shouldBe` 1
      it "(3,3)(2,3)==1" $ do distance (3, 3) (2, 3) `shouldBe` 1
      it "(3,3)(2,4)==1" $ do distance (3, 3) (2, 4) `shouldBe` 1
      it "(3,3)(2,5)==2" $ do distance (3, 3) (2, 5) `shouldBe` 2
      it "(3,3)(3,1)==2" $ do distance (3, 3) (3, 1) `shouldBe` 2
      it "(3,3)(3,2)==1" $ do distance (3, 3) (3, 2) `shouldBe` 1
      it "(3,3)(3,3)==0" $ do distance (3, 3) (3, 3) `shouldBe` 0
      it "(3,3)(3,4)==1" $ do distance (3, 3) (3, 4) `shouldBe` 1
      it "(3,3)(3,5)==2" $ do distance (3, 3) (3, 5) `shouldBe` 2
      it "(3,3)(4,1)==2" $ do distance (3, 3) (4, 1) `shouldBe` 2
      it "(3,3)(4,2)==1" $ do distance (3, 3) (4, 2) `shouldBe` 1
      it "(3,3)(4,3)==1" $ do distance (3, 3) (4, 3) `shouldBe` 1
      it "(3,3)(4,4)==1" $ do distance (3, 3) (4, 4) `shouldBe` 1
      it "(3,3)(4,5)==2" $ do distance (3, 3) (4, 5) `shouldBe` 2
      it "(3,3)(5,1)==2" $ do distance (3, 3) (5, 1) `shouldBe` 2
      it "(3,3)(5,2)==2" $ do distance (3, 3) (5, 2) `shouldBe` 2
      it "(3,3)(5,3)==2" $ do distance (3, 3) (5, 3) `shouldBe` 2
      it "(3,3)(5,4)==2" $ do distance (3, 3) (5, 4) `shouldBe` 2
      it "(3,3)(5,5)==2" $ do distance (3, 3) (5, 5) `shouldBe` 2
