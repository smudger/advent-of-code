module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Map qualified as M
import Data.Maybe

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve input = length . filter ((== 'S') . snd) . mapMaybe (hop gridMap) . filter ((== 'A') . snd) . mapMaybe (hop gridMap) . filter ((== 'M') . snd) . mapMaybe (hop gridMap) . mSearches $ gridToMap input
  where
    mSearches = concatMap (\(x, y) -> map (\d -> ((x, y, d), 'X')) [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]) . M.keys . M.filter (== 'X')
    gridMap = gridToMap input

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Show)

hop :: M.Map (Int, Int) Char -> ((Int, Int, Direction), Char) -> Maybe ((Int, Int, Direction), Char)
hop m ((x, y, d), _) = fmap ((x', y', d),) $ M.lookup (x', y') m
  where
    (x', y') = case d of
      North -> (x, y - 1)
      NorthEast -> (x + 1, y - 1)
      East -> (x + 1, y)
      SouthEast -> (x + 1, y + 1)
      South -> (x, y + 1)
      SouthWest -> (x - 1, y + 1)
      West -> (x - 1, y)
      NorthWest -> (x - 1, y - 1)

gridToMap :: String -> M.Map (Int, Int) Char
gridToMap = M.fromList . concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0 ..] . map (zip [0 ..]) . lines

example1 :: String
example1 = $(makeRelativeToProject "example1.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)
