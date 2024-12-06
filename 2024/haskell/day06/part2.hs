module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List
import Data.Maybe
import Data.Set qualified as S

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve x = length . map fst . filter (createsLoop startingObstacles dimension) $ potentialObstacles
  where
    startingObstacles = S.fromList . map fst . filter ((== '#') . snd) . toGrid $ x
    dimension = length . lines $ x
    startPoint = fst . fromJust . find ((== '^') . snd) . toGrid $ x
    potentialObstacles = drop 1 . nubBy (\a b -> fst a == fst b) $ mapRoute startingObstacles dimension (startPoint, North)

mapRoute :: S.Set Point -> Int -> (Point, Direction) -> [(Point, Direction)]
mapRoute obstacles dimension v@((x, y), _)
  | x < 0 || y < 0 || x >= dimension || y >= dimension = []
  | otherwise = v : mapRoute obstacles dimension (hop obstacles v)

createsLoop :: S.Set Point -> Int -> (Point, Direction) -> Bool
createsLoop obstacles dimension (p@(x, y), d) = case d of
  North -> isLoop (S.insert p obstacles) dimension ((x, y + 1), North) S.empty
  East -> isLoop (S.insert p obstacles) dimension ((x - 1, y), East) S.empty
  South -> isLoop (S.insert p obstacles) dimension ((x, y - 1), South) S.empty
  West -> isLoop (S.insert p obstacles) dimension ((x + 1, y), West) S.empty

isLoop :: S.Set Point -> Int -> (Point, Direction) -> S.Set (Point, Direction) -> Bool
isLoop obstacles dimension v@((x, y), _) seen
  | x < 0 || y < 0 || x >= dimension || y >= dimension = False
  | v `S.member` seen = True
  | otherwise = isLoop obstacles dimension (hop obstacles v) (S.insert v seen)

hop :: S.Set Point -> (Point, Direction) -> (Point, Direction)
hop obstacles ((x, y), direction) = case direction of
  North -> if (x, y - 1) `S.member` obstacles then hop obstacles ((x, y), East) else ((x, y - 1), North)
  East -> if (x + 1, y) `S.member` obstacles then hop obstacles ((x, y), South) else ((x + 1, y), East)
  South -> if (x, y + 1) `S.member` obstacles then hop obstacles ((x, y), West) else ((x, y + 1), South)
  West -> if (x - 1, y) `S.member` obstacles then hop obstacles ((x, y), North) else ((x - 1, y), West)

type Point = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

toGrid :: String -> [(Point, Char)]
toGrid = concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0 ..] . map (zip [0 ..]) . lines

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
