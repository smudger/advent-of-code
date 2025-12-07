module Main (main) where

import Solver (parse, part1, part2)

main :: IO ()
main = do
  (start, splitters, height) <- parse <$> getContents
  print (part1 start splitters height)
  print (part2 start splitters height)
