module Main (main) where

import Solver (parse1, parse2, solveAll)

main :: IO ()
main = do
  input <- getContents
  print (solveAll $ parse1 input)
  print (solveAll $ parse2 input)
