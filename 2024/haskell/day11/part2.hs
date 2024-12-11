module Main where

import Data.Char (isDigit)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Map (Map)
import Data.Map qualified as M

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: Input -> Int
solve = sum . M.elems . doTimes 75 applyBlink . M.fromList . map (,1) . nums

type Input = String

doTimes :: Int -> (a -> a) -> (a -> a)
doTimes n f x = iterate f x !! n

-- >>> applyBlink (M.fromList [(125, 2), (17, 1)])
-- fromList [(1,1),(7,1),(253000,2)]
applyBlink :: Map Int Int -> Map Int Int
applyBlink = M.unionsWith (+) . M.elems . M.mapWithKey blinkN

-- >>> blinkN 17 4
-- fromList [(1,4),(7,4)]
blinkN :: Int -> Int -> Map Int Int
blinkN x n = M.fromListWith (+) . map (,n) $ blink x

-- >>> blink 17
-- [1,7]
blink :: Int -> [Int]
blink x
  | x == 0 = [1]
  | (digits x) `mod` 2 == 0 = splitDigits x
  | otherwise = [x * 2024]

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
