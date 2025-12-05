module Solver where

import Data.Char (isDigit)
import Data.List (sort)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: [Range] -> [Int] -> Int
part1 fresh = length . filter (flip isFresh fresh)

part2 :: [Range] -> Int
part2 = sum . map lenRange . combineRanges

-- >>> parse "9-13\n4-5\n\n1\n2\n4"
-- ([(9,13),(4,5)],[1,2,4])
parse :: String -> ([Range], [Int])
parse input =
  let (rs, xs) = (break (== "")) $ lines input
      fresh = map range rs
      available = map read . drop 1 $ xs
   in (fresh, available)

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Range = (Int, Int)

-- >>> combineRanges [(3,5), (10,14), (16,20), (12,18)]
-- [(3,5),(10,20)]
combineRanges :: [Range] -> [Range]
combineRanges = foldl' combine [] . sort
  where
    combine [] y = [y]
    combine (x@(x1, x2) : xs) y@(y1, y2)
      | x1 <= y2 && y1 <= x2 = (min x1 y1, max x2 y2) : xs
      | otherwise = x : combine xs y

-- >>> lenRange (2, 4)
-- 3
lenRange :: Range -> Int
lenRange (x1, x2) = 1 + x2 - x1

-- >>> isFresh 2 [(13, 15), (1, 4)]
-- True
isFresh :: Int -> [Range] -> Bool
isFresh x = any (x `inRange`)

-- >>> 11 `inRange` (9, 13)
-- True
-- >>> 4 `inRange` (9, 13)
-- False
inRange :: Int -> Range -> Bool
inRange x (y1, y2) = x >= y1 && x <= y2

-- >>> range "9-13"
-- (9,13)
range :: String -> Range
range s =
  let (start, end) = span isDigit s
   in (read start, read (drop 1 end))
