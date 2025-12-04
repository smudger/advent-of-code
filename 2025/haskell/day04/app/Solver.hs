module Solver where

import Data.Set (Set)
import Data.Set qualified as S

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: Set (Int, Int) -> Int
part1 input = length . S.filter (not . isBlocked input) $ input

part2 :: Set (Int, Int) -> Int
part2 input =
  let n = S.size input
      n' = S.size (removeAllFreeRolls input)
   in n - n'

parse :: String -> Set (Int, Int)
parse input = S.fromList $ [(x, y) | (y, r) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] r, c == '@']

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

removeAllFreeRolls :: Set (Int, Int) -> Set (Int, Int)
removeAllFreeRolls xs =
  let n = S.size xs
      xs' = removeFreeRolls xs
      n' = S.size xs'
   in if n == n' then xs else removeAllFreeRolls xs'

removeFreeRolls :: Set (Int, Int) -> Set (Int, Int)
removeFreeRolls xs = S.filter (isBlocked xs) xs

isBlocked :: Set (Int, Int) -> (Int, Int) -> Bool
isBlocked xs = (>= 4) . length . occupiedNeighbours xs

occupiedNeighbours :: Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
occupiedNeighbours xs = filter ((flip S.member) xs) . neighbours
  where
    neighbours :: (Int, Int) -> [(Int, Int)]
    neighbours (x, y) =
      [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0]
