module Solver where

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: [Point] -> Int
part1 = maximum . map (uncurry area) . pairs

part2 :: [Point] -> Int
part2 _input = 0

-- >>> parse "7,1\n11,1"
-- [(7,1),(11,1)]
parse :: String -> [Point]
parse = map parseLine . lines
  where
    parseLine :: String -> Point
    parseLine l =
      let (x, y) = break (== ',') l
       in (read x, read $ drop 1 y)

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Point = (Int, Int)

-- >>> area (2, 5) (9, 7)
-- 24
-- >>> area (7, 3) (2, 3)
-- 6
area :: Point -> Point -> Int
area (x1, y1) (x2, y2) =
  let x = 1 + (abs $ x2 - x1)
      y = 1 + (abs $ y2 - y1)
   in x * y

-- >>> pairs [1, 2, 3]
-- [(1,2),(1,3),(2,3)]
pairs :: (Eq a) => [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = (map ((x,)) xs) ++ pairs xs
