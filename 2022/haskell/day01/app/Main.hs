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
calorieCounts = map (sum . map read) . partitionBy (== "") . lines

-- See source code for `words`
-- >>> partitionBy (=="") ["1", "2", "3", "", "4", "5"]
-- [["1","2","3"],["4","5"]]
partitionBy :: (a -> Bool) -> [a] -> [[a]]
partitionBy f xs = case dropWhile f xs of
  [] -> []
  xs' -> x : partitionBy f xs''
    where
      (x, xs'') = break f xs'
