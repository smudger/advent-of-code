module Main (main) where

import Data.List (sort)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 = maximum . map sumCalories . groupByElf . lines

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sumCalories . groupByElf . lines

-- >>> sumCalories ["1", "2", "3"]
-- 6
sumCalories :: [String] -> Int
sumCalories [] = 0
sumCalories (x : xs) = read x + sumCalories xs

-- >>> groupByElf ["1", "2", "", "3", "4", "5"]
-- [["1","2"],["3","4","5"]]
groupByElf :: [String] -> [[String]]
groupByElf [] = []
groupByElf (x : xs) =
  if x == ""
    then [] : groupByElf xs
    else prependToFirst x (groupByElf xs)

-- >>> prependToFirst 1 []
-- [[1]]
-- >>> prependToFirst 1 [[2, 2], [3, 3, 3]]
-- [[1,2,2],[3,3,3]]
prependToFirst :: a -> [[a]] -> [[a]]
prependToFirst x [] = [[x]]
prependToFirst x (y : ys) = (x : y) : ys
