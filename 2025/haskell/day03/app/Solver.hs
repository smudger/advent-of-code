module Solver where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

solve :: [[Int]] -> Solution
solve = foldl' update (MkSolution 0 0)
  where
    update :: Solution -> [Int] -> Solution
    update (MkSolution acc1 acc2) xs =
      let r1 = joltage . batteries 2 $ xs
          r2 = joltage . batteries 12 $ xs
       in (MkSolution (r1 + acc1) (r2 + acc2))

-- >>> parse "123\n456"
-- [[1,2,3],[4,5,6]]
parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

data Solution = MkSolution {part1 :: !Int, part2 :: !Int}

joltage :: [Int] -> Int
joltage = foldl' (\acc n -> n + (10 * acc)) 0

batteries :: Int -> [Int] -> [Int]
batteries 1 xs = [maximum xs]
batteries n xs =
  let window = (length xs) - (n - 1)
      x = maximum (take window xs)
      xs' = drop (1 + (fromJust $ elemIndex x xs)) xs
   in x : batteries (n - 1) xs'
