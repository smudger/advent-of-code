module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Foldable qualified as S
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

main :: IO ()
main = print . solve $ input

type Input = String

type Point = (Int, Int)

data Direction = North | South | East | West deriving (Ord, Eq, Show)

solve :: Input -> Int
solve x = length . S.map fst $ pointsOnShortestPath endWithDir walls costs
  where
    (start, end, walls) = parse x
    costs = dijkstra start walls
    endWithDir = fst . head . sortBy (compare `on` snd) . M.toList . M.filterWithKey (\(p, _) _ -> p == end) $ costs

pointsOnShortestPath :: (Point, Direction) -> Set Point -> Map (Point, Direction) Int -> Set (Point, Direction)
pointsOnShortestPath end walls costs = go (S.singleton end) [end]
  where
    go points [] = points
    go points (current : others) =
      let pres = filter (\n -> S.notMember (fst $ fst n) walls) $ predecessors current
          onPath = filter (\(loc, cost) -> cost + (costs M.! loc) == costs M.! current) pres
       in go (S.union points (S.fromList (map fst onPath))) ((map fst onPath) ++ others)

dijkstra :: Point -> Set Point -> Map (Point, Direction) Int
dijkstra start walls = dijkstra' (M.singleton (start, East) 0) S.empty [((start, East), 0)]
  where
    dijkstra' distances _ [] = distances
    dijkstra' distances visited toVisit =
      let ((currentLoc, currentDistance) : toVisit') = sortBy (compare `on` snd) toVisit
       in case (S.member currentLoc visited) of
            True -> dijkstra' distances visited toVisit'
            False ->
              let visited' = S.insert currentLoc visited
                  currentNeighbors = filter ((\p -> S.notMember p walls) . fst . fst) $ neighbors currentLoc
                  tentatives = map (\(p, d) -> (p, d + currentDistance)) currentNeighbors
                  distances' = M.unionWith (\a b -> if a < b then a else b) distances (M.fromList tentatives)
                  toVisit'' = toVisit' ++ tentatives
               in dijkstra' distances' visited' toVisit''

neighbors :: (Point, Direction) -> [((Point, Direction), Int)]
neighbors (p@(x, y), North) = [((p, East), 1000), ((p, West), 1000), (((x, y - 1), North), 1)]
neighbors (p@(x, y), East) = [((p, North), 1000), ((p, South), 1000), (((x + 1, y), East), 1)]
neighbors (p@(x, y), South) = [((p, East), 1000), ((p, West), 1000), (((x, y + 1), South), 1)]
neighbors (p@(x, y), West) = [((p, North), 1000), ((p, South), 1000), (((x - 1, y), West), 1)]

predecessors :: (Point, Direction) -> [((Point, Direction), Int)]
predecessors (p@(x, y), North) = [((p, East), 1000), ((p, West), 1000), (((x, y + 1), North), 1)]
predecessors (p@(x, y), East) = [((p, North), 1000), ((p, South), 1000), (((x - 1, y), East), 1)]
predecessors (p@(x, y), South) = [((p, East), 1000), ((p, West), 1000), (((x, y - 1), South), 1)]
predecessors (p@(x, y), West) = [((p, North), 1000), ((p, South), 1000), (((x + 1, y), West), 1)]

parse :: Input -> (Point, Point, Set Point)
parse i =
  let locs = [((x, y), c) | (y, row) <- zip [0 ..] (lines i), (x, c) <- zip [0 ..] row, c /= '.']
      walls = map fst . filter ((== '#') . snd) $ locs
      start = fromJust . listToMaybe . map fst . filter ((== 'S') . snd) $ locs
      end = fromJust . listToMaybe . map fst . filter ((== 'E') . snd) $ locs
   in (start, end, S.fromList walls)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
