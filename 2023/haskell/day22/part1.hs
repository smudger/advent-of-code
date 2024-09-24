module Main where

import Data.Char
import Data.Function
import Data.List
import Debug.Trace

example :: [String]
example =
  [ "1,0,1~1,2,1",
    "0,0,2~2,0,2",
    "0,2,3~2,2,3",
    "0,0,4~0,2,4",
    "2,0,5~2,2,5",
    "0,1,6~2,1,6",
    "1,1,8~1,1,9"
  ]

type Cube = (Int, Int, Int)

type Block = [Cube]

parseRow :: String -> Block
parseRow row
  | x1 < x2 = map (,y1,z1) [x1 .. x2]
  | x1 > x2 = map (,y1,z1) [x2 .. x1]
  | y1 < y2 = map (x1,,z1) [y1 .. y2]
  | y1 > y2 = map (x1,,z1) [y2 .. y1]
  | z1 < z2 = map (x1,y1,) [z1 .. z2]
  | z1 > z2 = map (x1,y1,) [z2 .. z1]
  | otherwise = [(x1, y1, z1)]
  where
    intsInString = map read . filter (isDigit . head) . groupBy ((==) `on` isDigit)
    [x1, y1, z1, x2, y2, z2] = intsInString row

dropUntilRest :: [Block] -> [Block]
dropUntilRest blocks
  | dropped == blocks = dropped
  | dropped /= blocks = dropUntilRest dropped
  where
    dropped = dropOnce blocks

dropOnce :: [Block] -> [Block]
dropOnce blocks
  | [] <- droppable = blocks
  | (b : _) <- droppable = map dropCube b : filter (b /=) blocks
  where
    allCubes = concat blocks
    canDrop block = all ((\b@(_, _, z) -> (b `notElem` allCubes || b `elem` block) && z > 0) . dropCube) block
    droppable = filter canDrop blocks

dropCube :: Cube -> Cube
dropCube (x, y, z) = (x, y, z - 1)

supportersForBlock :: [Block] -> Block -> [Block]
supportersForBlock others block =
  let loweredBlock = map dropCube block
   in filter (any (`elem` loweredBlock)) others

supporters :: [Block] -> [[Block]]
supporters blocks = map (\b -> supportersForBlock (filter (/= b) blocks) b) blocks

solve :: String -> Int
solve input =
  let blocks = map parseRow . lines $ input
      restedBlocks = dropUntilRest blocks
      supportingBlocks = nub . map head . filter ((==) 1 . length) . supporters $ restedBlocks
   in (length . lines $ input) - length supportingBlocks

main = readFile "input.txt" >>= print . solve