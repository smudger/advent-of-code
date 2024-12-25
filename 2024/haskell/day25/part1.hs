module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List
import Data.List.Split

main :: IO ()
main = print . solve $ input

solve :: Input -> Int
solve = length . (\(ks, ls) -> validCombinations ks ls) . keysAndLocks

type Input = String

type Lock = (Int, Int, Int, Int, Int)

type Key = (Int, Int, Int, Int, Int)

keysAndLocks :: Input -> ([Key], [Lock])
keysAndLocks = foldl' go ([], []) . splitOn "\n\n"
  where
    go (ks, ls) s = if isKey s then (parse s : ks, ls) else (ks, parse s : ls)

isKey :: String -> Bool
isKey = all (== '.') . head . lines

-- >>> parse ".....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####"
-- (5,0,2,1,3)
parse :: String -> Key
parse = (\(a : b : c : d : e : _) -> (a, b, c, d, e)) . map (length . filter (== '#')) . transpose . drop 1 . init . lines

-- >>> validCombinations [(5,0,2,1,3), (4,3,4,0,2), (3,0,2,0,1)] [(0,5,3,4,3), (1,2,0,5,3)]
-- [((4,3,4,0,2),(1,2,0,5,3)),((3,0,2,0,1),(0,5,3,4,3)),((3,0,2,0,1),(1,2,0,5,3))]
validCombinations :: [Key] -> [Lock] -> [(Key, Lock)]
validCombinations ks ls = filter (\(k, l) -> k `fits` l) [(k, l) | k <- ks, l <- ls]

-- >>> (5, 0, 2, 1, 3) `fits` (0, 5, 3, 4, 3)
-- False
-- >>> (3, 0, 2, 0, 1) `fits` (0, 5, 3, 4, 3)
-- True
fits :: Key -> Lock -> Bool
fits (k1, k2, k3, k4, k5) (l1, l2, l3, l4, l5) =
  all
    (<= 5)
    [ k1 + l1,
      k2 + l2,
      k3 + l3,
      k4 + l4,
      k5 + l5
    ]

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
