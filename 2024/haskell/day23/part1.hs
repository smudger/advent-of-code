module Main where

import Data.Char (isAsciiLower)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List
import Data.Set (Set)
import Data.Set qualified as S

main :: IO ()
main = print . solve $ input

solve :: Input -> Int
solve i = length . filter (isTriangle n) . filter (startsWithT) . setsOfThree . computers $ i
  where
    n = network i

type Input = String

type Computer = String

type Connection = (Computer, Computer)

-- >>> startsWithT ["ab", "cd", "te"]
-- True
-- >>> startsWithT ["ab", "cd", "et"]
-- False
startsWithT :: [Computer] -> Bool
startsWithT = any ((== 't') . head)

-- >>> computers "ab-cd\nef-ab"
-- ["ab","cd","ef"]
computers :: Input -> [Computer]
computers = nub . chunksOf 2 . filter isAsciiLower

-- >>> setsOfThree ["ab", "cd", "ef", "gh"]
-- [["ab","cd","ef"],["ab","cd","gh"],["ab","ef","gh"],["cd","ef","gh"]]
setsOfThree :: [Computer] -> [[Computer]]
setsOfThree xs = [a : b : c : [] | (a : as) <- tails xs, (b : bs) <- tails as, (c : _) <- tails bs]

-- >>> network "ab-cd\nef-ab"
-- [("ab","cd"),("ef","ab")]
network :: Input -> Set Connection
network = S.fromList . map (pair . chunksOf 2) . chunksOf 4 . filter isAsciiLower
  where
    pair (a : b : _) = (a, b)
    pair _ = error "Invalid input"

-- >>> isTriangle [("ab", "cd"), ("ab", "ef"), ("ef", "cd")] ["ab", "cd", "ef"]
-- True
-- >>> isTriangle [("ab", "cd"), ("ab", "ef"), ("ef", "gh")] ["ab", "cd", "ef"]
-- False
isTriangle :: Set Connection -> [Computer] -> Bool
isTriangle n (a : b : c : _) =
  (== 3) . length . filter (\x -> S.member x n) $
    [ (a, b),
      (b, a),
      (a, c),
      (c, a),
      (b, c),
      (c, b)
    ]
isTriangle _ _ = error "Wrong number of vertices"

-- >>> chunksOf 2 "abcdef"
-- ["ab","cd","ef"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
