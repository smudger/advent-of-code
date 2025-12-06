module Solver where

import Data.List (foldl1', transpose)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

solveAll :: [Equation] -> Int
solveAll = sum . map solve
  where
    solve :: Equation -> Int
    solve = uncurry foldl1'

parse1 :: String -> [Equation]
parse1 = map equation . transpose . map words . lines
  where
    equation :: [String] -> Equation
    equation xs =
      let op = parseOp (last xs)
          nums = map read $ init xs
       in (op, nums)

parse2 :: String -> [Equation]
parse2 input = map parseEq (equations input)
  where
    equations :: String -> [[String]]
    equations eqs =
      let opsLine = last (lines eqs)
          starts :: [Int]
          starts = [i | (c, i) <- zip opsLine [0 ..], c /= ' ']
          ranges :: [(Int, Int)]
          ranges = zip starts (drop 1 starts ++ [length opsLine + 1])
       in map extractEq ranges
      where
        extractEq :: (Int, Int) -> [String]
        extractEq (s, e) = map (take (e - (s + 1)) . drop s) . lines $ eqs
    parseEq :: [String] -> Equation
    parseEq s =
      let op = parseOp (last s)
          nums = map read . transpose . init $ s
       in (op, nums)

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Operation = Int -> Int -> Int

type Equation = (Operation, [Int])

parseOp :: String -> Operation
parseOp ('*' : _) = (*)
parseOp ('+' : _) = (+)
parseOp op = error ("Unknown operation: " ++ op)
