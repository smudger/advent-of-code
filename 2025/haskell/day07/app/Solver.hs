module Solver where

import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: Point -> Set Point -> Int -> Int
part1 start splitters height = fst $ foldl' doLevel (0, S.singleton start) [0 .. (height - 1)]
  where
    doLevel :: (Int, Set Point) -> Int -> (Int, Set Point)
    doLevel (n, beams) l =
      let beams' = S.map (\(x, _) -> (x, l)) beams
          (n', beams'') = foldl' handleBeam (0, S.empty) beams'
       in (n + n', beams'')
    handleBeam :: (Int, Set Point) -> Point -> (Int, Set Point)
    handleBeam (n, acc) b@(bx, by)
      | S.member b splitters = (n + 1, S.union acc (S.fromList [(bx - 1, by), (bx + 1, by)]))
      | otherwise = (n, S.insert b acc)

part2 :: Point -> Set Point -> Int -> Int
part2 start splitters height = sum $ foldl' doLevel (M.singleton start 1) [0 .. (height - 1)]
  where
    doLevel :: Map Point Int -> Int -> Map Point Int
    doLevel beams l =
      let beams' = M.mapKeys (\(x, _) -> (x, l)) beams
          beams'' = M.foldlWithKey' handleBeam M.empty beams'
       in beams''
    handleBeam :: Map Point Int -> Point -> Int -> Map Point Int
    handleBeam acc b@(bx, by) n
      | S.member b splitters = M.unionWith (+) acc (M.fromList [((bx - 1, by), n), ((bx + 1, by), n)])
      | otherwise = M.insertWith (+) b n acc

parse :: String -> (Point, Set Point, Int)
parse s =
  let start = case enumerate 'S' s of
        [x] -> x
        _ -> error "Could not find a single start point"
      splitters = S.fromList $ enumerate '^' s
   in (start, splitters, length $ lines s)

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Point = (Int, Int)

-- >>> enumerate 'S' "...\n..S\nS.."
-- [(2,1),(0,2)]
enumerate :: Char -> String -> [Point]
enumerate c s =
  [ (x, y)
  | (r, y) <- zip (lines s) [0 ..],
    (c', x) <- zip r [0 ..],
    c == c'
  ]

-- >>> nextSplitter 10 (St.fromList [(1, 2), (2, 5), (2, 7)]) (2, 2)
-- Just (2,5)
-- >>> nextSplitter 10 (St.fromList [(1, 2), (2, 5), (2, 7)]) (2, 9)
-- Nothing
nextSplitter :: Int -> Set Point -> Point -> Maybe Point
nextSplitter height splitters =
  find (`S.member` splitters)
    . takeWhile (\(_, y) -> y < height)
    . iterate (\(x, y) -> (x, y + 1))
