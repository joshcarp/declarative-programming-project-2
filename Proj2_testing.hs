module Test where
import Proj2
import Test.QuickCheck
import Text.Printf
import Data.Char
import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set


prop_abs :: Int -> Int
prop_abs n = n

main :: IO ()
main =
    testCheckLocation >>
    testToLocation >>
    testDistance >>
    testFeedback >>
    testCheckLocationToInt
--    testValidLocations

-- QuickCheck Tests
testCheckLocation = quickCheck (withMaxSuccess 100 checkLocation)

checkLocation ::  Char -> Int -> Property
checkLocation x y =   elem x ['A' .. 'H']
         ==> ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
         === Just (printf "%c%d" x y)


testCheckLocationToInt = quickCheck (withMaxSuccess 100 checkLocationInt)

checkLocationInt :: Int -> Property
checkLocationInt x = x > 0 ==> locationToInt (intToLocation x) === x


-- Unit Tests

toloc x = case toLocation x of
                    Just y -> y
                    Nothing -> (-99, -99) :: Location

testFeedback = hspec $ do
          describe "testFeedback" $ do
            it "feedback" $ do
                feedback [(toloc "H1"), (toloc "B2"), (toloc "D3")] [(toloc "B3"), (toloc "C3"), (toloc "H3")] `shouldBe` (0, 2, 1)
            it "feedback" $ do
                feedback [(toloc "H1"), (toloc "B2"), (toloc "D3")] [(toloc "B1"), (toloc "A2"), (toloc "H3")] `shouldBe` (0, 2, 1)
            it "feedback" $ do
                feedback [(toloc "H1"), (toloc "B2"), (toloc "D3")] [(toloc "B2"), (toloc "H2"), (toloc "H1")] `shouldBe` (2, 1, 0)
            it "feedback" $ do
                feedback [(toloc "A1"), (toloc "D2"), (toloc "B3")] [(toloc "A3"), (toloc "D2"), (toloc "H1")] `shouldBe` (1, 1, 0)
            it "feedback" $ do
                feedback [(toloc "A1"), (toloc "D2"), (toloc "B3")] [(toloc "H4"), (toloc "G3"), (toloc "H2")] `shouldBe` (0, 0, 0)
            it "feedback" $ do
                feedback [(toloc "A1"), (toloc "D2"), (toloc "B3")] [(toloc "D2"), (toloc "B3"), (toloc "A1")] `shouldBe` (3, 0, 0)

--
--testValidLocations = hspec $ do
--          describe "testValidLocations" $ do
--            it "testValidLocations" $ do
--                validLocations (mustToLocation "A1") (mustToLocation "A1") `shouldBe` Set.fromList [(1, 1)]
--            it "testValidLocations" $ do
--                validLocations (mustToLocation "A1") (mustToLocation "H4") `shouldBe` Set.fromList [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4),(5,1),(5,2),(5,3),(5,4),(6,1),(6,2),(6,3),(6,4),(7,1),(7,2),(7,3),(7,4),(8,1),(8,2),(8,3),(8,4)]
--


testToLocation = hspec $ do
          describe "testToLocation" $ do
            it "toLocation" $ do
              (toLocation "a2") `shouldBe` Just ((1, 2) :: Location)

{-
Grid for distance tests
    1  2  3  4  5
-------------------
1 | 2  2  2  2  2
2 | 2  1  1  1  2
3 | 2  1  0  1  2
4 | 2  1  1  1  2
5 | 2  2  2  2  2


0 0 0
0 o x
0 0 o
-}

testDistance = hspec $ do
          describe "testDistance" $ do
            it "(3, 3) (1, 1) == 2" $ do
                distance (3, 3) (1, 1) `shouldBe` 2
            it "(3, 3) (1, 2) == 2" $ do
                distance (3, 3) (1, 2) `shouldBe` 2
            it "(3, 3) (1, 3) == 2" $ do
                distance (3, 3) (1, 3) `shouldBe` 2
            it "(3, 3) (1, 4) == 2" $ do
                distance (3, 3) (1, 4) `shouldBe` 2
            it "(3, 3) (1, 5) == 2" $ do
                distance (3, 3) (1, 5) `shouldBe` 2

            it "(3, 3) (2, 1) == 2" $ do
                distance (3, 3) (2, 1) `shouldBe` 2
            it "(3, 3) (2, 2) == 1" $ do
                distance (3, 3) (2, 2) `shouldBe` 1
            it "(3, 3) (2, 3) == 1" $ do
                distance (3, 3) (2, 3) `shouldBe` 1
            it "(3, 3) (2, 4) == 1" $ do
                distance (3, 3) (2, 4) `shouldBe` 1
            it "(3, 3) (2, 5) == 2" $ do
                distance (3, 3) (2, 5) `shouldBe` 2

            it "(3, 3) (3, 1) == 2" $ do
                distance (3, 3) (3, 1) `shouldBe` 2
            it "(3, 3) (3, 2) == 1" $ do
                distance (3, 3) (3, 2) `shouldBe` 1
            it "(3, 3) (3, 3) == 0" $ do
                distance (3, 3) (3, 3) `shouldBe` 0
            it "(3, 3) (3, 4) == 1" $ do
                distance (3, 3) (3, 4) `shouldBe` 1
            it "(3, 3) (3, 5) == 2" $ do
                distance (3, 3) (3, 5) `shouldBe` 2

            it "(3, 3) (4, 1) == 2" $ do
                distance (3, 3) (4, 1) `shouldBe` 2
            it "(3, 3) (4, 2) == 1" $ do
                distance (3, 3) (4, 2) `shouldBe` 1
            it "(3, 3) (4, 3) == 1" $ do
                distance (3, 3) (4, 3) `shouldBe` 1
            it "(3, 3) (4, 4) == 1" $ do
                distance (3, 3) (4, 4) `shouldBe` 1
            it "(3, 3) (4, 5) == 2" $ do
                distance (3, 3) (4, 5) `shouldBe` 2

            it "(3, 3) (5, 1) == 2" $ do
                distance (3, 3) (5, 1) `shouldBe` 2
            it "(3, 3) (5, 2) == 2" $ do
                distance (3, 3) (5, 2) `shouldBe` 2
            it "(3, 3) (5, 3) == 2" $ do
                distance (3, 3) (5, 3) `shouldBe` 2
            it "(3, 3) (5, 4) == 2" $ do
                distance (3, 3) (5, 4) `shouldBe` 2
            it "(3, 3) (5, 5) == 2" $ do
                distance (3, 3) (5, 5) `shouldBe` 2


{-
def cal_average(num):
    sum_num = 0
    for t in num:
        sum_num = sum_num + t
    avg = sum_num / len(num)
    return avg
-}