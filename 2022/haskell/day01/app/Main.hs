{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List (sort)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 = maximum . map sumCalories . elves . lines

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map sumCalories . elves . lines

-- >>> sumCalories ["1", "2", "3"]
-- 6
sumCalories :: [String] -> Int
sumCalories = foldl' (\acc x -> acc + read x) 0

-- See source code for `words`
-- >>> elves ["", "", "1", "2", "", "", "", "3", "4", "5", "", "6", ""]
-- [["1","2"],["3","4","5"],["6"]]
elves :: [String] -> [[String]]
elves xs = case dropWhile (== "") xs of
  [] -> []
  xs' -> x : elves xs''
    where
      (x, xs'') = break (== "") xs'
