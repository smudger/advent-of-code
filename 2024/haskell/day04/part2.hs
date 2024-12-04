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
solve input = length . filter (\cs -> all ($ cs) [correctLetterPermutation, containsTwo 'M', containsTwo 'S']) . map (toCorners gridMap) $ centers
  where
    correctLetterPermutation = (< 4) . length . group
    containsTwo c = (== 2) . length . filter (== c)
    centers = M.keys . M.filter (== 'A') $ gridMap
    gridMap = gridToMap input

type Point = (Int, Int)

toCorners :: M.Map Point Char -> Point -> [Char]
toCorners m (x, y) = mapMaybe (\p -> M.lookup p m) [(x - 1, y - 1), (x + 1, y - 1), (x + 1, y + 1), (x - 1, y + 1)]

gridToMap :: String -> M.Map Point Char
gridToMap = M.fromList . concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0 ..] . map (zip [0 ..]) . lines

example1 :: String
example1 = $(makeRelativeToProject "example1.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

example3 :: String
example3 = $(makeRelativeToProject "example3.txt" >>= embedStringFile)
