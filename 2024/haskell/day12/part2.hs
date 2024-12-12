module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = sum . map price . concatMap (contiguousBy inRegion) . groupByPlotType . plots

type Input = String

type Point = (Int, Int)

-- >>> plots "AB\nAC"
-- [((0,0),'A'),((1,0),'B'),((0,1),'A'),((1,1),'C')]
plots :: Input -> [(Point, Char)]
plots input = [((x, y), c) | (y, r) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] r]

-- >>> groupByPlotType [((0, 0), 'A'), ((0, 1), 'B'), ((1, 0), 'A')]
-- [[(0,0),(1,0)],[(0,1)]]
groupByPlotType :: [(Point, Char)] -> [[Point]]
groupByPlotType = map (map fst) . groupBy ((==) `on` snd) . sortBy (compare `on` snd)

-- >>> contiguousBy inRegion [(1, 2), (1, 3), (4, 5), (6, 7)]
-- [[(6,7)],[(4,5)],[(1,3),(1,2)]]
contiguousBy :: (Eq a) => (a -> [a] -> Bool) -> [a] -> [[a]]
contiguousBy f =
  let contiguousBy' rs p = case filter (f p) rs of
        [] -> [p] : rs
        rs' -> (p : fold rs') : filter (\r -> not $ any (== r) rs') rs
   in foldl' contiguousBy' []

-- >>> inRegion (1, 2) [(1, 5), (1, 4), (1, 3)]
-- True
-- >>> inRegion (1, 2) [(1, 6), (1, 5), (1, 4)]
-- False
inRegion :: Point -> [Point] -> Bool
inRegion p = any (isNeighbour p)

-- >>> price [(0, 1), (0, 2), (1, 1), (1, 2)]
-- 16
price :: [Point] -> Int
price r = area r * perimeter r

-- >>> area [(1, 1), (1, 2)]
-- 2
area :: [Point] -> Int
area = length

-- >>> perimeter [(2, 1), (2, 2), (3, 2), (3, 3)]
-- 8
perimeter :: [Point] -> Int
perimeter r = length . (contiguousBy inLine) . concatMap (exteriorFences r) $ r

data Side = North | East | South | West deriving (Eq, Show)

type Fence = (Point, Side)

-- >>> inLine ((1, 2), South) [((2, 2), South)]
-- True
-- >>> inLine ((1, 2), North) [((2, 2), South)]
-- False
inLine :: Fence -> [Fence] -> Bool
inLine (p, s) fs = any (\(p', s') -> s == s' && isNeighbour p p') fs

-- >>> exteriorFences [(1, 2), (0, 1)] (1, 1)
-- [((1,1),East),((1,1),North)]
exteriorFences :: [Point] -> Point -> [Fence]
exteriorFences r p =
  mapMaybe
    id
    [ check (1, 0) East,
      check (-1, 0) West,
      check (0, 1) South,
      check (0, -1) North
    ]
  where
    check p' s = if (p .+. p') `elem` r then Nothing else Just (p, s)

-- >>> isNeighbour (1, 1) (1, 0)
-- True
isNeighbour :: Point -> Point -> Bool
isNeighbour p1 p2 = p1 `elem` neighbours p2

-- >>> neighbours (1, 2)
-- [(2,2),(0,2),(1,3),(1,1)]
neighbours :: Point -> [Point]
neighbours x = map (.+. x) directions

directions :: [Point]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- >>> (1, 2) .+. (2, 3)
-- (3,5)
(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

example3 :: String
example3 = $(makeRelativeToProject "example3.txt" >>= embedStringFile)

example4 :: String
example4 = $(makeRelativeToProject "example4.txt" >>= embedStringFile)

example5 :: String
example5 = $(makeRelativeToProject "example5.txt" >>= embedStringFile)
