module Solver where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (find, partition, sortBy, tails, union)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: [Point] -> Int
part1 xs =
  let distances :: [((Point, Point), Double)]
      distances = take (if length xs < 1000 then 10 else 1000) . sortBy (compare `on` snd) . concatMap calcDistances . tails $ xs
      circuits :: [[Point]]
      circuits = connect $ map fst distances
   in product . map length . take 3 . reverse . sortBy (compare `on` length) $ circuits

part2 :: [Point] -> Int
part2 xs =
  let distances :: [(Point, Point)]
      distances = map fst . sortBy (compare `on` snd) . concatMap calcDistances . tails $ xs
      xs' = S.fromList xs
      n' = fromJust $ find (\n -> head (connectN n distances) == xs') [(length xs) .. (length xs) ^ (2 :: Integer)]
      ((x1, _, _), (x2, _, _)) = distances !! (n' - 1)
   in x1 * x2

parse :: String -> [Point]
parse = map point . lines
  where
    point :: String -> Point
    point s = case nums s of
      [x, y, z] -> (x, y, z)
      _ -> error "could not parse line"

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

type Point = (Int, Int, Int)

nums :: String -> [Int]
nums s = case dropWhile (not . isDigit) s of
  [] -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . isDigit) s'

calcDistances :: [Point] -> [((Point, Point), Double)]
calcDistances [] = []
calcDistances (x : ys) = map (\y -> ((x, y), distance x y)) ys

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) =
  let x = fromIntegral $ (x2 - x1) ^ (2 :: Integer)
      y = fromIntegral $ (y2 - y1) ^ (2 :: Integer)
      z = fromIntegral $ (z2 - z1) ^ (2 :: Integer)
   in sqrt $ x + y + z

connect :: [(Point, Point)] -> [[Point]]
connect [] = []
connect ((p1, p2) : ps) =
  let cs = connect ps
      (matches, cs') = partition (\c -> p1 `elem` c || p2 `elem` c) cs
      new = case matches of
        [] -> [p1, p2]
        [c] -> union c [p1, p2]
        [c1, c2] -> union c1 c2
        _ -> error "too many matches"
   in new : cs'

connect' :: [(Point, Point)] -> [Set Point]
connect' = foldl' update []
  where
    update :: [Set Point] -> (Point, Point) -> [Set Point]
    update cs (p1, p2) =
      let (matches, cs') = partition (\c -> S.member p1 c || S.member p2 c) cs
          new = case matches of
            [] -> S.fromList [p1, p2]
            [c] -> S.union c (S.fromList [p1, p2])
            [c1, c2] -> S.union c1 c2
            _ -> error "too many matches"
       in new : cs'

connectN :: Int -> [(Point, Point)] -> [Set Point]
connectN n = connect' . take n