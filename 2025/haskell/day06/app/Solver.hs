module Solver where

import Data.List (foldl1', transpose)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

solveAll :: [Equation] -> Int
solveAll = sum . map solve

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
parse2 = map equation . transpose . map words . lines
  where
    equation :: [String] -> Equation
    equation xs =
      let nums = parseNums (init xs)
       in case last xs of
            "*" -> ((*), nums)
            "+" -> ((+), nums)
            _ -> undefined
    parseNums :: [String] -> [Int]
    parseNums = map read

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Operation = Int -> Int -> Int

type Equation = (Operation, [Int])

solve :: Equation -> Int
solve = uncurry foldl1'
