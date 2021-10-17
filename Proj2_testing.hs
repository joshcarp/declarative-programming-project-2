module Test where
import Proj2
import Test.QuickCheck
import Text.Printf
import Data.Char
import Test.Hspec
import Test.QuickCheck


prop_abs :: Int -> Int
prop_abs n = n

main :: IO ()
main = testCheckLocation >>
       testToLocation >>
       testDistance >>
       testDistancex >>
       testDistancexx

-- QuickCheck Tests
testCheckLocation = quickCheck checkLocation

checkLocation ::  Char -> Int -> Property
checkLocation x y =   elem x ['A' .. 'H']
         ==> ((toLocation (printf "%c%d" x y)) >>= (\r -> return (fromLocation r)))
         === Just (printf "%c%d" x y)

-- Unit Tests
testToLocation = hspec $ do
          describe "testToLocation" $ do
            it "toLocation" $ do
              (toLocation "a2") `shouldBe` Just ((1, 2) :: Location)


testDistancex = hspec $ do
                        describe "testDistancexx" $ do
                          it "distancex [(3, 3)] (3, 3) 0 == 1" $ do
                            (distancex [(3, 3)] (3, 3) 0) `shouldBe` 1
                          it "distancex [(3, 3)] (3, 4) 0 == 0" $ do
                            (distancex [(3, 3)] (3, 4) 0) `shouldBe` 0
                          it "distancex [(3, 3)] (3, 4) 1 == 1" $ do
                            (distancex [(3, 3)] (3, 4) 1) `shouldBe` 1
                          it "distancex [(3, 3)] (3, 4) 1 == 1" $ do
                            (distancex [(3, 3), (6, 6)] (3, 4) 1) `shouldBe` 1

testDistancexx = hspec $ do
                        describe "testDistancex" $ do
                          it "distancexx [(3, 3)] (3, 3) 0 == 1" $ do
                            (distancexx [(3, 3)] [(3, 3)] 0) `shouldBe` 1
                          it "distancexx [(3, 3)] (3, 4) 0 == 0" $ do
                            (distancexx [(3, 3)] [(3, 4)] 0) `shouldBe` 0
                          it "distancexx [(3, 3)] (3, 4) 1 == 1" $ do
                            (distancexx [(3, 3)] [(3, 4)] 1) `shouldBe` 1

{-
Grid for distance tests
    1  2  3  4  5
-------------------
1 | 2  2  2  2  2
2 | 2  1  1  1  2
3 | 2  1  0  1  2
4 | 2  1  1  1  2
5 | 2  2  2  2  2

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


