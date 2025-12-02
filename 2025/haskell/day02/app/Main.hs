module Main (main) where

import Solver (Solution (MkSolution), parse, solve)

main :: IO ()
main = do
  input <- parse <$> getContents
  let (MkSolution sol1 sol2) = solve input
  print sol1
  print sol2
