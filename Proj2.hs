module Proj2 where
import Data.Char
import Text.Printf
import qualified Data.Set as Set

type Location  = (Int, Int)
type GameState = Int

toLocation :: String -> Maybe Location
toLocation (x:xs) = Just ((ord (toLower x) - ord 'a' + 1), (read xs :: Int))
toLocation _      = Nothing

fromLocation :: Location -> String
fromLocation (x, y) = (printf "%c%d" (toUpper (chr (ord 'a' + x - 1))) y)

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback x [y1, y2, y3] = (zeroDist, oneDist, twoDist)
                where zeroDist = sum [1 | minimum (distancex x y1) == 0] + sum [1 | minimum (distancex x y2) == 0] + sum [1 | minimum (distancex x y3) == 0]
                      oneDist = sum [1 | minimum (distancex x y1) == 1] + sum [1 | minimum (distancex x y2) == 1] + sum [1 | minimum (distancex x y3) == 1]
                      twoDist = sum [1 | minimum (distancex x y1) == 2] + sum [1 | minimum (distancex x y2) == 2] + sum [1 | minimum (distancex x y3) == 2]

distancex a b = [distance c b | c <- a]

distance :: (Real a, Real a, Integral b) => (a, a) -> (a, a) -> b
distance (x1, y1) (x2, y2) = floor( sqrt (realToFrac(((realToFrac x1) - (realToFrac x2))^2 + ((realToFrac y1) - (realToFrac y2))^2)))

initialGuess :: ([Location],GameState)
initialGuess = ([(1, 1)], 1)

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess _ _ = ([(1, 1)], 1)


