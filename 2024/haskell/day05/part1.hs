module Main where

import Data.Char
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve x = sum . map middle . filter (isValid (rules x)) . updates $ x

type Rule = (Int, Int)

isValid :: [Rule] -> [Int] -> Bool
isValid ruleset update = all (\rule -> isValid' rule update) ruleset
  where
    isValid' r u = case (elemIndex (fst r) u, elemIndex (snd r) u) of
      (Just i, Just j) -> i < j
      _ -> True

rules :: String -> [Rule]
rules = map ((\(a : b : _) -> (a, b)) . nums) . takeWhile (/= "") . lines

updates :: String -> [[Int]]
updates = map nums . dropWhile (notElem ',') . lines

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

-- See day03/part1.hs
nums :: String -> [Int]
nums s = case dropWhile (not . isDigit) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . isDigit) s'

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
