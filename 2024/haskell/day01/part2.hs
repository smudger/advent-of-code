module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (group, sort, transpose)
import Data.Map qualified as M

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve x = sum . M.elems . M.mapWithKey (\k v -> k * v * M.findWithDefault 0 k colB) $ colA
  where
    (colA : colB : _) = map counts . transpose . map parse . lines $ x
    parse = map (read @Int) . words
    counts = M.fromList . map (\l@(a : _) -> (a, length l)) . group . sort

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
