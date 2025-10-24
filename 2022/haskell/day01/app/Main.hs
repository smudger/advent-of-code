module Main (main) where

import Solver (part1, part2)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)
