module Main where

import Data.Char (isAsciiLower)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Function (on)
import Data.List

main :: IO ()
main = print . solve $ input

solve :: Input -> String
solve i = intercalate "," . sort $ bronKerbosch (network i) [] (computers i) []

type Input = String

type Computer = String

type Connection = (Computer, Computer)

--- >>> bronKerbosch [("6", "4"), ("4", "5"), ("4", "3"), ("5", "2"), ("5", "1"), ("3", "2"), ("2", "1")] [] ["6", "5", "4", "3", "2", "1"] []
-- ["1","2","5"]
bronKerbosch :: [Connection] -> [Computer] -> [Computer] -> [Computer] -> [Computer]
bronKerbosch _ r [] _ = r
bronKerbosch g r ps@(p : _) xs = last . sortBy (compare `on` length) . map go $ zip3 nonNeighboursOfP (map (ps \\) $ inits nonNeighboursOfP) (map (union xs) $ inits nonNeighboursOfP)
  where
    nonNeighboursOfP = filter (not . isNeighbour g p) ps
    go (p', ps', xs') = bronKerbosch g (p' : r) (filter (isNeighbour g p') ps') (filter (isNeighbour g p') xs')

-- >>> isNeighbour [("a", "b")] "b" "a"
-- True
-- >>> isNeighbour [("a", "b")] "x" "a"
-- False
isNeighbour :: [Connection] -> Computer -> Computer -> Bool
isNeighbour [] _ _ = False
isNeighbour (x : xs) a b =
  if x == (a, b) || x == (b, a)
    then True
    else isNeighbour xs a b

-- >>> computers "ab-cd\nef-ab"
-- ["ab","cd","ef"]
computers :: Input -> [Computer]
computers = nub . chunksOf 2 . filter isAsciiLower

-- >>> network "ab-cd\nef-ab"
-- [("ab","cd"),("ef","ab")]
network :: Input -> [Connection]
network = map (pair . chunksOf 2) . chunksOf 4 . filter isAsciiLower
  where
    pair (a : b : _) = (a, b)
    pair _ = error "Invalid input"

-- >>> chunksOf 2 "abcdef"
-- ["ab","cd","ef"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
