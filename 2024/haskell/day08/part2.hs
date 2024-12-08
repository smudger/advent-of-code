module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Function (on)
import Data.List (groupBy, nub, sortBy)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

type Input = String

solve :: Input -> Int
solve input = length . nub . concatMap (concatMap (antinodesInGrid 0 max) . intoPairs) . intoGroups . antennas $ input
  where
    max = (length $ lines input) - 1

type Point = (Int, Int)

antennas :: Input -> [(Point, Char)]
antennas i = [((x, y), c) | (y, r) <- zip [0 ..] (lines i), (x, c) <- zip [0 ..] r, c /= '.']

-- >>> intoGroups [((1, 0), 'A'), ((2, 3), 'B'), ((3, 4), 'A')]
-- [[(1,0),(3,4)],[(2,3)]]
intoGroups :: [(Point, Char)] -> [[Point]]
intoGroups = map (map fst) . groupBy ((==) `on` snd) . sortBy (compare `on` snd)

-- >>> intoPairs [(0, 0), (1, 1), (2, 2)]
-- [((0,0),(1,1)),((0,0),(2,2)),((1,1),(2,2))]
intoPairs :: [Point] -> [(Point, Point)]
intoPairs [] = []
intoPairs (p1 : ps) = [(p1, p2) | p2 <- ps] ++ intoPairs ps

-- >>> inGrid 0 4 (2, 3)
-- True
inGrid :: Int -> Int -> Point -> Bool
inGrid min max (x, y) = and [x >= min, x <= max, y >= min, y <= max]

-- >>> antinodesInGrid 0 7 ((4, 3), (5, 5))
-- [(4,3),(3,1),(5,5),(6,7)]
antinodesInGrid :: Int -> Int -> (Point, Point) -> [Point]
antinodesInGrid min max (p1, p2) = whileInGrid fromP1 ++ whileInGrid fromP2
  where
    fromP1 = goFrom p1 (gradient p2 p1)
    fromP2 = goFrom p2 (gradient p1 p2)
    whileInGrid = takeWhile (inGrid min max)

-- >>> take 4 $ goFrom (0, 0) (1, 2)
-- [(0,0),(1,2),(2,4),(3,6)]
goFrom :: Point -> Point -> [Point]
goFrom p v = iterate ((.+.) v) p

-- >>> gradient (1, 2) (3, 5)
-- (2,3)
gradient :: Point -> Point -> Point
gradient p1 p2 = p2 .-. p1

-- >>> (4, 6) .-. (1, 2)
-- (3,4)
(.-.) :: Point -> Point -> Point
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)

-- >>> (1, 2) .+. (3, 4)
-- (4,6)
(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x2 + x1, y2 + y1)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
