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
solve x =
  let parse = map (read @Int) . words
      counts = map (\l@(x : _) -> (x, length l)) . group . sort
      (first : xs) = map counts . transpose . map parse . lines $ x
      (second : _) = xs
      secondMap = M.fromList second
   in sum $ map (\(num, count) -> num * count * (M.findWithDefault 0 num secondMap)) first

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
