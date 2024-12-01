module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Ix (rangeSize)
import Data.List (sort, transpose)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = sum . map (\(a, b) -> abs $ b - a) . zipFromArray . map sort . transpose . map parse . lines
  where
    parse = map (read @Int) . words
    zipFromArray xs = zip (head xs) (head $ drop 1 xs)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
