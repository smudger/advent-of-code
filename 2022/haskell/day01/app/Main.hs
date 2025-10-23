{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List (sort)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 = maximum . calorieCounts

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . calorieCounts

-- >>> calorieCounts "5\n7\n3\n\n4\n5"
-- [15,9]
calorieCounts :: String -> [Int]
calorieCounts = map (sum . map read) . partitionOn "" . lines

-- See source code for `words`
-- >>> partitionOn "" ["1", "2", "3", "", "4", "5"]
-- [["1","2","3"],["4","5"]]
partitionOn :: (Eq a) => a -> [a] -> [[a]]
partitionOn y xs = case dropWhile (== y) xs of
  [] -> []
  xs' -> x : partitionOn y xs''
    where
      (x, xs'') = break (== y) xs'
