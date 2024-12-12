module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Foldable
import Data.Function
import Data.List

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = sum . map price . concatMap intoContiguous . groupByPlotType . plots

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

-- >>> intoContiguous [(1, 2), (1, 5), (4, 5), (6, 7)]
-- [[(6,7)],[(4,5)],[(1,5)],[(1,2)]]
intoContiguous :: [Point] -> [[Point]]
intoContiguous = foldl' assignToRegion []

-- >>> assignToRegion [[(1, 2), (2, 2)], [(4, 5)]] (2, 3)
-- [[(2,3),(1,2),(2,2)],[(4,5)]]
-- >>> assignToRegion [[(1, 2), (2, 2)], [(4, 5)]] (6, 7)
-- [[(6,7)],[(1,2),(2,2)],[(4,5)]]
assignToRegion :: [[Point]] -> Point -> [[Point]]
assignToRegion rs p = case filter (inRegion p) rs of
  [] -> [p] : rs
  rs' -> (p : fold rs') : filter (\r -> not $ any (== r) rs') rs

-- >>> inRegion (1, 2) [(1, 5), (1, 4), (1, 3)]
-- True
-- >>> inRegion (1, 2) [(1, 6), (1, 5), (1, 4)]
-- False
inRegion :: Point -> [Point] -> Bool
inRegion p = any (isNeighbour p)

-- >>> price [(0, 1), (0, 2), (1, 1), (1, 2)]
-- 32
price :: [Point] -> Int
price r = area r * perimeter r

-- >>> area [(1, 1), (1, 2)]
-- 2
area :: [Point] -> Int
area = length

-- >>> perimeter [(2, 1), (2, 2), (3, 2), (3, 3)]
-- 10
perimeter :: [Point] -> Int
perimeter r = sum $ map (length . filter (\n -> notElem n r) . neighbours) r

-- >>> exteriorEdges [(1, 2), (2, 2), (1, 3)] (1, 2)
-- [(0,2),(1,1)]
exteriorEdges :: [Point] -> Point -> [Point]
exteriorEdges r = filter (\n -> notElem n r) . neighbours

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
