module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (sort, transpose)
import Data.List.NonEmpty as NEL (group, head, length)
import Data.Map qualified as M

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve x = sum . M.elems . M.mapWithKey (\k v -> k * v * M.findWithDefault 0 k colBCounts) $ colACounts
  where
    (colACounts : colBCounts : _) = map elemCounts . transpose . map parse . lines $ x
    parse = map read . words
    elemCounts = M.fromList . map (\l -> (NEL.head l, NEL.length l)) . NEL.group . sort

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
