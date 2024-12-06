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
solve = length . nub . mapRoute

mapRoute :: String -> [Point]
mapRoute x = followPath obstacles dimension (startPoint, North)
  where
    obstacles = S.fromList . map fst . filter ((== '#') . snd) . toGrid $ x
    startPoint = fst . fromJust . find ((== '^') . snd) . toGrid $ x
    dimension = length . lines $ x

followPath :: S.Set Point -> Int -> (Point, Direction) -> [Point]
followPath obstacles dimension v@((x, y), _)
  | x < 0 || y < 0 || x >= dimension || y >= dimension = []
  | otherwise = (x, y) : followPath obstacles dimension (hop obstacles v)

hop :: S.Set Point -> (Point, Direction) -> (Point, Direction)
hop obstacles ((x, y), direction) = case direction of
  North -> if (x, y - 1) `S.member` obstacles then hop obstacles ((x, y), East) else ((x, y - 1), North)
  East -> if (x + 1, y) `S.member` obstacles then hop obstacles ((x, y), South) else ((x + 1, y), East)
  South -> if (x, y + 1) `S.member` obstacles then hop obstacles ((x, y), West) else ((x, y + 1), South)
  West -> if (x - 1, y) `S.member` obstacles then hop obstacles ((x, y), North) else ((x - 1, y), West)

type Point = (Int, Int)

data Direction = North | East | South | West deriving (Show)

toGrid :: String -> [(Point, Char)]
toGrid = concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) . zip [0 ..] . map (zip [0 ..]) . lines

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
