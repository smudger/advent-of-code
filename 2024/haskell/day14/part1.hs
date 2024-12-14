module Main where

import Data.Char
import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = print . solve $ input

xDim :: Int
xDim = 101

yDim :: Int
yDim = 103

solve :: Input -> Int
solve = product . map length . quadrants . map (posAt 100) . robots

type Input = String

type Position = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Position, Velocity)

robots :: Input -> [Robot]
robots = map (toRobot . nums) . lines
  where
    toRobot (px : py : vx : vy : _) = ((px, py), (vx, vy))
    toRobot _ = error "parsing failed"

-- >>> quadrants [(0, 2), (1, 3), (1, 6), (6, 0), (6, 0)]
-- [[(0,2)],[(6,0),(6,0)],[(1,6)],[]]
quadrants :: [Position] -> [[Position]]
quadrants ps =
  let midX = xDim `div` 2
      midY = yDim `div` 2
   in [ filter (\(x, y) -> x < midX && y < midY) ps,
        filter (\(x, y) -> x > midX && y < midY) ps,
        filter (\(x, y) -> x < midX && y > midY) ps,
        filter (\(x, y) -> x > midX && y > midY) ps
      ]

-- >>> posAt 2 ((2, 4), (2, -3))
-- (6,5)
posAt :: Int -> Robot -> Position
posAt t ((px, py), (vx, vy)) = (px', py')
  where
    px' = (px + (vx * t)) `mod` xDim
    py' = (py + (vy * t)) `mod` yDim

-- >>> nums "p=0,4 v=3,-3"
-- [0,4,3,-3]
nums :: String -> [Int]
nums s = case dropWhile (not . liftA2 (||) isDigit (== '-')) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . liftA2 (||) isDigit (== '-')) s'

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
