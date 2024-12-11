module Main where

import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: Input -> Int
solve = length . doTimes 25 blink . nums

type Input = String

doTimes :: Int -> (a -> a) -> a -> a
doTimes n f x = iterate f x !! n

-- >>> blink [125, 17]
-- [253000,1,7]
blink :: [Int] -> [Int]
blink [] = []
blink (x : xs)
  | x == 0 = 1 : blink xs
  | (digits x) `mod` 2 == 0 = splitDigits x ++ blink xs
  | otherwise = (x * 2024) : blink xs

-- >>> splitDigits 1734
-- [17,34]
-- >>> splitDigits 1000
-- [10,0]
splitDigits :: Int -> [Int]
splitDigits n = [left, right]
  where
    divisor = 10 ^ ((digits n) `div` 2)
    left = n `div` divisor
    right = n - (left * divisor)

-- >>> digits 99
-- 2
digits :: Int -> Int
digits = ceiling . logBase (10.0 :: Double) . fromIntegral . (+ 1)

-- >>> nums "1 23 470"
-- [1,23,470]
nums :: String -> [Int]
nums s = case dropWhile (not . isDigit) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . isDigit) s'

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)
