module Solver where

import Data.Char (isDigit)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

solve :: [Int] -> Solution
solve = foldl' update (MkSolution 0 0)
  where
    update :: Solution -> Int -> Solution
    update (MkSolution s1 s2) x =
      let rs = repeats x
          s1' = if isInvalid1 rs then x + s1 else s1
          s2' = if isInvalid2 rs then x + s2 else s2
       in (MkSolution s1' s2')
    isInvalid1 :: [[String]] -> Bool
    isInvalid1 = any ((== 2) . length)
    isInvalid2 :: [[String]] -> Bool
    isInvalid2 = not . null

parse :: String -> [Int]
parse = concat . ranges . nums
  where
    ranges :: [Int] -> [[Int]]
    ranges (n1 : n2 : ns) = [n1 .. n2] : ranges ns
    ranges _ = []
    nums :: String -> [Int]
    nums s = case dropWhile (not . isDigit) s of
      [] -> []
      s' -> (read n) : nums s''
        where
          (n, s'') = break (not . isDigit) s'

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

data Solution = MkSolution {part1 :: !Int, part2 :: !Int}

-- >>> repeats 45454545
-- [["45","45","45","45"],["4545","4545"]]
repeats :: Int -> [[String]]
repeats n =
  let n' = show n
      l = length n'
      divisors = [i | i <- [1 .. l `div` 2], l `mod` i == 0]
   in filter allEqual . map (flip chunks n') $ divisors
  where
    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks i xs = (take i xs) : chunks i (drop i xs)
    allEqual :: (Eq a) => [a] -> Bool
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs
