module Solver where

import Data.Char (isDigit)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (find, nub, nubBy, sort)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: [(IntMap Light, [WiringSchematic])] -> Int
part1 = sum . map (uncurry solve)

part2 :: [(IntMap Light, [WiringSchematic])] -> Int
part2 _input = 0

parse :: String -> [(IntMap Light, [WiringSchematic])]
parse = map parseLine . lines
  where
    parseLine :: String -> (IntMap Light, [WiringSchematic])
    parseLine s =
      let ws = words s
          lights = IM.fromList . zip [0 ..] . map parseLight . drop 1 . init . head $ ws
          schematic = map nums . drop 1 . init $ ws
       in (lights, schematic)
    parseLight :: Char -> Light
    parseLight '.' = Off
    parseLight '#' = On
    parseLight _ = error "unrecognised light"
    nums :: String -> [Int]
    nums s = case dropWhile (not . isDigit) s of
      [] -> []
      s' -> (read n) : nums s''
        where
          (n, s'') = break (not . isDigit) s'

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

data Light = Off | On deriving (Show, Eq)

type WiringSchematic = [Int]

-- >>> solve (IM.fromList [(0, Off), (1, On), (2, On), (3, Off)]) [[3], [1,3], [2], [2,3], [0,2], [0,1]]
-- 2
-- >>> solve (IM.fromList [(0, Off), (1, Off), (2, Off), (3, On), (4, Off)]) [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]]
-- 3
solve :: IntMap Light -> [WiringSchematic] -> Int
solve goal ws = solveAux 0
  where
    solveAux :: Int -> Int
    solveAux n = case trySolve n goal ws of
      Nothing -> solveAux (n + 1)
      Just _ -> n

-- >>> trySolve 2 (IM.fromList [(0, Off), (1, On), (2, On), (3, Off)]) [[3], [1,3], [2], [2,3], [0,2], [0,1]]
-- Just [[1,3],[2,3]]
trySolve :: Int -> IntMap Light -> [WiringSchematic] -> Maybe [WiringSchematic]
trySolve n goal ws = find test $ pushes n ws
  where
    test :: [WiringSchematic] -> Bool
    test cs = goal == pushAll (IM.map (\_ -> Off) goal) cs

pushAll :: IntMap Light -> [WiringSchematic] -> IntMap Light
pushAll = foldr push

-- >>> push [1,3] (IM.fromList [(0, Off), (1, Off), (2, Off), (3, Off)])
-- fromList [(0,Off),(1,On),(2,Off),(3,On)]
push :: WiringSchematic -> IntMap Light -> IntMap Light
push [] ls = ls
push (w : ws) ls = IM.adjust toggle w (push ws ls)

toggle :: Light -> Light
toggle Off = On
toggle On = Off

-- >>> pushes 0 [1, 2, 3]
-- [[]]
-- >>> pushes 1 [1, 2, 3]
-- [[1],[2],[3]]
-- >>> pushes 2 [1, 2, 3]
-- [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
-- >>> pushes 3 [1, 2, 3]
-- [[1,1,1],[1,1,2],[1,1,3],[1,2,2],[1,2,3],[1,3,3],[2,2,2],[2,2,3],[2,3,3],[3,3,3]]
pushes :: (Eq a, Ord a) => Int -> [a] -> [[a]]
pushes 0 _ = [[]]
pushes n as = nub . map sort $ [a : bs | a <- as, bs <- pushes (n - 1) as]
