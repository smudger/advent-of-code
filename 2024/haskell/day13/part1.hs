module Main where

import Data.Char
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List

main :: IO ()
main = print . solve $ input

type Input = String

type Eqn = (Float, Float, Float)

type SimEqns = (Eqn, Eqn)

type Sln = (Float, Float)

solve :: Input -> Int
solve = sum . map tokens . filter isValid . map (solution . reduce) . equations

-- >>> equations "A: 94, 34\n B: 22, 67\n P: 8400, 5400"
-- [((94.0,22.0,8400.0),(34.0,67.0,5400.0))]
equations :: Input -> [SimEqns]
equations = map (toTuple2 . map toTuple3 . transpose) . chunksOf 3 . chunksOf 2 . nums

-- >>> reduce ((2.0, 4.0, 8.0), (3.0, 9.0, 15.0))
-- ((1.0,2.0,4.0),(0.0,1.0,1.0))
reduce :: SimEqns -> SimEqns
reduce (eq1@(x1, _, _), eq2@(x2, _, _)) =
  let eq1' = eq1 ./. (x1, x1, x1)
      eq2'@(_, y2', _) = eq2 .-. (eq1' .*. (x2, x2, x2))
      eq2'' = eq2' ./. (y2', y2', y2')
   in (eq1', eq2'')

-- >>> solution ((1.0, 2.0, 4.0), (0.0, 1.0, 1.0))
-- (2.0,1.0)
solution :: SimEqns -> Sln
solution ((_, y1, z1), (_, _, z2)) = (z1 - (y1 * z2), z2)

-- >>> tokens (80.0, 40.0)
-- 280
tokens :: Sln -> Int
tokens (x, y) = 3 * (round x) + (round y)

-- >>> isValid (1.0, 2.0)
-- True
-- >>> isValid (1.12, 2.0)
-- False
isValid :: Sln -> Bool
isValid (x, y) =
  all
    id
    [ isInt 3 x,
      isInt 3 y,
      x >= 0.0,
      y >= 0.0,
      x <= 100.0,
      y <= 100.0
    ]

-- >>> isInt 3 2.0
-- True
-- >>> isInt 3 2.12
-- False
-- >>> isInt 3 34.000008
-- True
-- >>> isInt 3 4.0000014
-- True
isInt :: Int -> Float -> Bool
isInt precision n = round (10 ^ fromIntegral precision * (n - fromIntegral (round n))) == 0

(./.) :: Eqn -> Eqn -> Eqn
(x1, y1, z1) ./. (x2, y2, z2) = (x1 / x2, y1 / y2, z1 / z2)

(.*.) :: Eqn -> Eqn -> Eqn
(x1, y1, z1) .*. (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

(.-.) :: Eqn -> Eqn -> Eqn
(x1, y1, z1) .-. (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- >>> toTuple2 [1, 2]
-- (1,2)
toTuple2 :: [a] -> (a, a)
toTuple2 (x : y : _) = (x, y)
toTuple2 _ = error "Undefined"

-- >>> toTuple3 [1, 2, 3]
-- (1,2,3)
toTuple3 :: [a] -> (a, a, a)
toTuple3 (x : y : z : _) = (x, y, z)
toTuple3 _ = error "Undefined"

-- >>> chunksOf 3 [1, 2, 3, 4, 5, 6]
-- [[1,2,3],[4,5,6]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

-- >>> nums "Button A: X+23, Y+470\nButton B: X+47, Y+360"
-- [23.0,470.0,47.0,360.0]
nums :: String -> [Float]
nums s = case dropWhile (not . isDigit) s of
  "" -> []
  s' -> (read n) : nums s''
    where
      (n, s'') = break (not . isDigit) s'

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
