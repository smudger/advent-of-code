module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, listToMaybe)

main :: IO ()
main = print . solve $ input

solve :: Input -> Int
solve x = sum . map gpsCoordinate . M.keys . M.filter (== BlockLeft) . snd $ foldl' move initial ds
  where
    (initial, ds) = parse x

type Input = String

type Point = (Int, Int)

data Direction = North | East | South | West deriving (Eq, Show)

data Item = BlockLeft | BlockRight | Wall | Empty deriving (Eq, Show)

type Warehouse = Map Point Item

type State = (Point, Warehouse)

parse :: Input -> (State, [Direction])
parse i =
  let i' = splitOn "\n\n" i
   in (parseState . fromJust . listToMaybe $ i', parseDirections . fromJust . listToMaybe . drop 1 $ i')

parseState :: String -> State
parseState i =
  let entries = [((x, y), c) | (y, row) <- zip [0 ..] (lines i), (x, c) <- zip [0, 2 ..] row]
   in (fst . fromJust . listToMaybe $ filter (\(_, c) -> c == '@') entries, M.fromList $ concatMap (\((x, y), c) -> [((x, y), fst $ parseItem c), ((x + 1, y), snd $ parseItem c)]) entries)

parseItem :: Char -> (Item, Item)
parseItem '#' = (Wall, Wall)
parseItem '.' = (Empty, Empty)
parseItem '@' = (Empty, Empty)
parseItem 'O' = (BlockLeft, BlockRight)
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
        Just _ -> moveBlocks robotLoc direction warehouse

moveBlocks :: Point -> Direction -> Warehouse -> State
moveBlocks robotLoc direction warehouse = case push (step direction robotLoc) direction warehouse of
  Left _ -> (robotLoc, warehouse)
  Right warehouse' -> ((step direction robotLoc), warehouse')

push :: Point -> Direction -> Warehouse -> Either String Warehouse
push p d w = case w M.! p of
  Wall -> Left "Can't push"
  Empty -> Right w
  BlockLeft -> case d of
    North -> fmap (M.insert (step d $ step East p) BlockRight . M.insert (step d p) BlockLeft . M.insert (step East p) Empty . M.insert p Empty) $ push (step d p) d w >>= push (step d $ step East p) d
    South -> fmap (M.insert (step d $ step East p) BlockRight . M.insert (step d p) BlockLeft . M.insert (step East p) Empty . M.insert p Empty) $ push (step d p) d w >>= push (step d $ step East p) d
    West -> error "Pushing left side west"
    East -> fmap (M.insert (step d $ step East p) BlockRight . M.insert (step d p) BlockLeft . M.insert p Empty) $ push (step d $ step East p) d w
  BlockRight -> case d of
    North -> fmap (M.insert (step d $ step West p) BlockLeft . M.insert (step d p) BlockRight . M.insert (step West p) Empty . M.insert p Empty) $ push (step d p) d w >>= push (step d $ step West p) d
    South -> fmap (M.insert (step d $ step West p) BlockLeft . M.insert (step d p) BlockRight . M.insert (step West p) Empty . M.insert p Empty) $ push (step d p) d w >>= push (step d $ step West p) d
    East -> error "Pushing right side east"
    West -> fmap (M.insert (step d $ step West p) BlockLeft . M.insert (step West p) BlockRight . M.insert p Empty) $ push (step d $ step West p) d w

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

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
