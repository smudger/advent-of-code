module Main (main) where

import Solver (parse, part1, part2)

main :: IO ()
main = do
  input <- parse <$> getContents
  print (part1 input)
  print (part2 input)
