module Main (main) where

import Solver (parse, part1, part2)

main :: IO ()
main = do
  (fresh, available) <- parse <$> getContents
  print (part1 fresh available)
  print (part2 fresh)
