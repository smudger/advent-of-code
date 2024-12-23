module Main where

import Data.Char (isAsciiLower)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List
import Data.Maybe (fromJust)

main :: IO ()
main = print . solve $ input

solve :: Input -> String
solve i = intercalate "," . sort . fromJust $ bronKerbosch (network i) [] (computers i) []

type Input = String

type Computer = String

type Connection = (Computer, Computer)

--- >>> bronKerbosch [("6", "4"), ("4", "5"), ("4", "3"), ("5", "2"), ("5", "1"), ("3", "2"), ("2", "1")] [] ["6", "5", "4", "3", "2", "1"] []
-- Just ["1","2","5"]
bronKerbosch :: [Connection] -> [Computer] -> [Computer] -> [Computer] -> Maybe [Computer]
bronKerbosch _ rs [] [] = Just rs
bronKerbosch _ _ [] _ = Nothing
bronKerbosch g rs ps@(p : _) xs = (\(a, _, _) -> a) $ foldl' go (Nothing, ps, xs) (filter (not . isNeighbour g p) ps)
  where
    go (Nothing, ps', xs') p' = (bronKerbosch g (p' : rs) (filter (isNeighbour g p) ps) (filter (isNeighbour g p') xs), filter (/= p') ps', p' : xs')
    go (Just c, ps', xs') p' =
      ( case bronKerbosch g (p' : rs) (filter (isNeighbour g p') ps) (filter (isNeighbour g p') xs) of
          Nothing -> Just c
          Just c' -> if length c' > length c then Just c' else Just c,
        filter (/= p') ps',
        p' : xs'
      )

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
network :: Input -> [Connection]
network = map (pair . chunksOf 2) . chunksOf 4 . filter isAsciiLower
  where
    pair (a : b : _) = (a, b)
    pair _ = error "Invalid input"

-- >>> isTriangle [("ab", "cd"), ("ab", "ef"), ("ef", "cd")] ["ab", "cd", "ef"]
-- True
-- >>> isTriangle [("ab", "cd"), ("ab", "ef"), ("ef", "gh")] ["ab", "cd", "ef"]
-- False
isTriangle :: [Connection] -> [Computer] -> Bool
isTriangle n (a : b : c : _) =
  (== 3) . length . filter (\x -> x `elem` n) $
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
