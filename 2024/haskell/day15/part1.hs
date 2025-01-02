module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, listToMaybe)

main :: IO ()
main = print . solve $ input

solve :: Input -> Int
solve x = sum . map gpsCoordinate . M.keys . M.filter (== Block) . snd $ foldl' move initial ds
  where
    (initial, ds) = parse x

type Input = String

type Point = (Int, Int)

data Direction = North | East | South | West deriving (Eq, Show)

data Item = Block | Wall | Empty deriving (Eq, Show)

type Warehouse = Map Point Item

type State = (Point, Warehouse)

parse :: Input -> (State, [Direction])
parse i =
  let i' = splitOn "\n\n" i
   in (parseState . fromJust . listToMaybe $ i', parseDirections . fromJust . listToMaybe . drop 1 $ i')

parseState :: String -> State
parseState i =
  let entries = [((x, y), c) | (y, row) <- zip [0 ..] (lines i), (x, c) <- zip [0 ..] row]
   in (fst . fromJust . listToMaybe $ filter (\(_, c) -> c == '@') entries, M.fromList $ map (\(p, c) -> (p, parseItem c)) entries)

parseItem :: Char -> Item
parseItem '#' = Wall
parseItem '.' = Empty
parseItem '@' = Empty
parseItem 'O' = Block
parseItem _ = error "Unknown item"

parseDirections :: String -> [Direction]
parseDirections [] = []
parseDirections ('^' : xs) = North : parseDirections xs
parseDirections ('>' : xs) = East : parseDirections xs
parseDirections ('v' : xs) = South : parseDirections xs
parseDirections ('<' : xs) = West : parseDirections xs
parseDirections ('\n' : xs) = parseDirections xs
parseDirections _ = error "Unknown direction"

move :: State -> Direction -> State
move (robotLoc, warehouse) direction =
  let newRobotLoc = step direction robotLoc
   in case M.lookup newRobotLoc warehouse of
        Nothing -> error "Unknown robot location"
        Just Wall -> (robotLoc, warehouse)
        Just Empty -> (newRobotLoc, warehouse)
        Just Block -> moveBlocks robotLoc direction warehouse

moveBlocks :: Point -> Direction -> Warehouse -> State
moveBlocks robotLoc direction warehouse =
  let line = takeWhile ((/= Wall) . (M.!) warehouse) $ iterate (step direction) (step direction robotLoc)
      gap = listToMaybe $ filter ((== Empty) . (M.!) warehouse) line
      newRobotLoc = step direction robotLoc
   in case gap of
        Nothing -> (robotLoc, warehouse)
        Just p -> (newRobotLoc, M.insert p Block (M.insert newRobotLoc Empty warehouse))

-- >>> step East (3, 4)
-- (4,4)
step :: Direction -> Point -> Point
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

-- >>> gpsCoordinate (4, 1)
-- 104
gpsCoordinate :: Point -> Int
gpsCoordinate (x, y) = x + (y * 100)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
