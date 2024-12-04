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
solve input = length . keep 'S' . hop . keep 'A' . hop . keep 'M' . hop . keep 'X' $ allVectorChars
  where
    keep c = filter ((== c) . snd)
    hop = mapMaybe (hop' gridMap)
    allVectorChars = concatMap (\(p, c) -> map (\d -> ((p, d), c)) [(minBound :: Direction) ..]) . M.toList $ gridMap
    gridMap = gridToMap input

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Show, Enum, Bounded)

type Point = (Int, Int)

type Vector = (Point, Direction)

hop' :: M.Map Point Char -> (Vector, Char) -> Maybe (Vector, Char)
hop' m (((x, y), d), _) = fmap ((point', d),) $ M.lookup point' m
  where
    point' = case d of
      North -> (x, y - 1)
      NorthEast -> (x + 1, y - 1)
      East -> (x + 1, y)
      SouthEast -> (x + 1, y + 1)
      South -> (x, y + 1)
      SouthWest -> (x - 1, y + 1)
      West -> (x - 1, y)
      NorthWest -> (x - 1, y - 1)

gridToMap :: String -> M.Map Point Char
gridToMap = M.fromList . concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0 ..] . map (zip [0 ..]) . lines

example1 :: String
example1 = $(makeRelativeToProject "example1.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)
