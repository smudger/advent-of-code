module Solver where

import Data.List (sort)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: String -> Integer
part1 input = case map countCalories (elves input) of
  [] -> 0
  xs -> maximum xs

part2 :: String -> Integer
part2 = sumLargestN 3 . map countCalories . elves

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

-- >>> sumLargestN 3 [6, 3, 2, 3, 1, 4, 5]
-- 15
sumLargestN :: (Ord a, Num a) => Int -> [a] -> a
sumLargestN n = sum . take n . reverse . sort

-- >>> countCalories ["1", "2", "3"]
-- 6
countCalories :: [String] -> Integer
countCalories = foldl' (\acc x -> acc + read x) 0

-- >>> elves "1\n2\n\n3\n"
-- [["1","2"],["3"]]
elves :: String -> [[String]]
elves = partitionBy (== "") . lines

-- See source code for `words`
-- >>> partitionBy (=="") ["1", "2", "3", "", "4", "5"]
-- [["1","2","3"],["4","5"]]
partitionBy :: (a -> Bool) -> [a] -> [[a]]
partitionBy f xs = case dropWhile f xs of
  [] -> []
  xs' -> x : partitionBy f xs''
    where
      (x, xs'') = break f xs'