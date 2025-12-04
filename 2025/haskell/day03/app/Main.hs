module Main (main) where

import Solver (Solution (part1, part2), parse, solve)

main :: IO ()
main = do
  input <- parse <$> getContents
  let solution = solve input
  print (part1 solution)
  print (part2 solution)
