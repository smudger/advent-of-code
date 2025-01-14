module Main where

import Control.Applicative
import Data.Char
import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = sum . map head . filter isSolvable . map nums . lines

-- >>> isSolvable [190, 10, 19]
-- True
-- >>> isSolvable [83, 17, 5]
-- False
isSolvable :: [Int] -> Bool
isSolvable (expected : xs) = any (== expected) . map (calculate xs) $ operatorsOf (length xs - 1)

-- >>> calculate [2, 3, 4] [(*), (+)]
-- 10
-- >>> calculate [2, 3, 4] [(*), (.||.)]
-- 64
calculate :: [Int] -> [Int -> Int -> Int] -> Int
calculate (start : xs) = foldl (\x f -> f x) start . combine xs

-- >>> combine [2, 3] [(*), (+)]
-- [(*2), (+3)]
combine :: [Int] -> [Int -> Int -> Int] -> [Int -> Int]
combine (x : xs) (f : fs) = f x : combine xs fs
combine _ _ = []

-- ALTERNATIVELY, using applicative functors
-- combine xs fs = getZipList $ ($) <$> ZipList fs <*> ZipList xs

-- >>> operatorsOf 2
-- [[(+),(+)],[(+),(*)],[(+),(.||.)],[(*),(+)],[(*),(*)],[(*),(.||.)],[(.||.),(+)],[(.||.),(*)],[(.||.),(.||.)]]
operatorsOf :: Int -> [[Int -> Int -> Int]]
operatorsOf = sequencesOf [(+), (*), (.||.)]

-- >>> (.||.) 6 15
-- 156
(.||.) :: Int -> Int -> Int
(.||.) a b = b * (10 ^ digitCount a) + a
  where
    digitCount = floor . (+ 1) . logBase (10.0 :: Double) . fromIntegral

-- >>> sequencesOf [(+), (*)] 2
-- [[(+),(+)],[(+),(*)],[(*),(+)],[(*),(*)]]
sequencesOf :: [a] -> Int -> [[a]]
sequencesOf _ 0 = [[]]
sequencesOf xs n = [x : ys | x <- xs, ys <- sequencesOf xs (n - 1)]

-- See day03/part1.hs
nums :: String -> [Int]
nums s = case dropWhile (not . isDigit) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . isDigit) s'

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
