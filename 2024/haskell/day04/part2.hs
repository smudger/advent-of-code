module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List
import Data.Map qualified as M
import Data.Maybe

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve input = length . filter ((< 4) . length . group) . filter ((== 2) . length . filter (== 'M')) . filter ((== 2) . length . filter (== 'S')) . filter ((== 4) . length) . map (filter (\c -> 'M' == c || 'S' == c) . mapMaybe id . tips gridMap) $ centers
  where
    centers = M.keys . M.filter (== 'A') $ gridMap
    gridMap = gridToMap input

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Show, Enum, Bounded)

type Point = (Int, Int)

tips :: M.Map Point Char -> Point -> [Maybe Char]
tips m (x, y) = map (\p -> M.lookup p m) [(x - 1, y - 1), (x + 1, y - 1), (x + 1, y + 1), (x - 1, y + 1)]

gridToMap :: String -> M.Map Point Char
gridToMap = M.fromList . concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0 ..] . map (zip [0 ..]) . lines

example1 :: String
example1 = $(makeRelativeToProject "example1.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

example3 :: String
example3 = $(makeRelativeToProject "example3.txt" >>= embedStringFile)
