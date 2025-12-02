module Solver where

import Data.Char (isDigit)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: [Int] -> Int
part1 = sum . filter isInvalid1

part2 :: [Int] -> Int
part2 = sum . filter isInvalid2

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

-- >>> isInvalid1 446446
-- True
-- >>> isInvalid1 134
-- False
isInvalid1 :: Int -> Bool
isInvalid1 n =
  let n' = show n
      mid = (length n') `div` 2
   in take mid n' == drop mid n'

-- >>> isInvalid2 121212
-- True
-- >>> isInvalid2 121313
-- False
isInvalid2 :: Int -> Bool
isInvalid2 n =
  let n' = show n
      l = length n'
      divisors = [i | i <- [1 .. l `div` 2], l `mod` i == 0]
   in any allEqual . map (flip chunks n') $ divisors
  where
    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks i xs = (take i xs) : chunks i (drop i xs)
    allEqual :: (Eq a) => [a] -> Bool
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs
