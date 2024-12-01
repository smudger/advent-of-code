module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (sort, transpose)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = sum . map absDiff . transpose . map sort . transpose . map parse . lines
  where
    parse = map read . words
    absDiff = abs . foldl1 (-)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
