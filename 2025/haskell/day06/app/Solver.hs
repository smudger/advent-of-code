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
      let nums = map read $ init xs
       in case last xs of
            "*" -> ((*), nums)
            "+" -> ((+), nums)
            _ -> undefined

parse2 :: String -> [Equation]
parse2 input = map parseEq (equations input)

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Operation = Int -> Int -> Int

type Equation = (Operation, [Int])

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
  let op = case last s of
        (o : _) -> parseOp o
        _ -> undefined
      nums = map read . filter (not . all (== ' ')) . transpose . init $ s
   in (op, nums)

parseOp :: Char -> Operation
parseOp '*' = (*)
parseOp '+' = (+)
parseOp _ = undefined
