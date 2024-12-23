module Main where

import Data.Char (isAsciiLower)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Function (on)
import Data.List
import Data.Set (Set)
import Data.Set qualified as S

main :: IO ()
main = print . solve $ input

solve :: Input -> String
solve i = intercalate "," . sort $ bronKerbosch (network i) [] (computers i) []

type Input = String

type Computer = String

type Connection = (Computer, Computer)

--- >>> bronKerbosch (S.fromList [("6", "4"), ("4", "5"), ("4", "3"), ("5", "2"), ("5", "1"), ("3", "2"), ("2", "1")]) [] ["6", "5", "4", "3", "2", "1"] []
-- ["1","2","5"]
bronKerbosch :: Set Connection -> [Computer] -> [Computer] -> [Computer] -> [Computer]
bronKerbosch _ clique [] _ = clique
bronKerbosch graph clique untestedNeighbours@(pivot : _) testedNeighbours = last . sortBy (compare `on` length) . map (\(r, p, x) -> bronKerbosch graph r p x) $ testCases
  where
    neighboursTestCases = filter (not . isNeighbour graph pivot) untestedNeighbours
    untestedNeighboursTestCases = map (untestedNeighbours \\) . inits $ neighboursTestCases
    testedNeighboursTestCases = map (union testedNeighbours) . inits $ neighboursTestCases
    testCases = map testCase $ zip3 neighboursTestCases untestedNeighboursTestCases testedNeighboursTestCases
    testCase (neighbour', untestedNeighbours', testedNeighbours') = (neighbour' : clique, filter (isNeighbour graph neighbour') untestedNeighbours', filter (isNeighbour graph neighbour') testedNeighbours')

-- >>> isNeighbour (S.fromList[("a", "b")]) "b" "a"
-- True
-- >>> isNeighbour (S.fromList [("a", "b")]) "x" "a"
-- False
isNeighbour :: Set Connection -> Computer -> Computer -> Bool
isNeighbour xs a b = S.member (a, b) xs || S.member (b, a) xs

-- >>> computers "ab-cd\nef-ab"
-- ["ab","cd","ef"]
computers :: Input -> [Computer]
computers = nub . chunksOf 2 . filter isAsciiLower

-- >>> network "ab-cd\nef-ab"
-- [("ab","cd"),("ef","ab")]
network :: Input -> Set Connection
network = S.fromList . map (pair . chunksOf 2) . chunksOf 4 . filter isAsciiLower
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
