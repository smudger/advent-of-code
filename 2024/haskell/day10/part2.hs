module Main where

import Data.Char (digitToInt)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Map (Map)
import Data.Map qualified as M

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: Input -> Int
solve input = sum $ map (trailsToSummit tmap) trailheads
  where
    tmap = toTopMap input
    trailheads = M.keys $ M.filter (== 0) tmap

type Input = String

type Point = (Int, Int)

type TopMap = Map Point Int

type Trail = [Point]

trailsToSummit :: TopMap -> Point -> Int
trailsToSummit tmap p
  | isSummit tmap p = 1
  | otherwise = sum . map (trailsToSummit tmap) . filter (canStep tmap p) $ stepsFrom p

-- >>> isSummit (M.fromList [((1, 1), 9)]) (1, 1)
-- True
isSummit :: TopMap -> Point -> Bool
isSummit tmap p = (M.lookup p tmap) == Just 9

-- >>> canStep (M.fromList [((1, 1), 1), ((1, 2), 2)]) (1, 1) (1, 2)
-- True
canStep :: TopMap -> Point -> Point -> Bool
canStep tmap from to = case (M.lookup from tmap, M.lookup to tmap) of
  (Just a, Just b) -> b == a + 1
  _ -> False

-- >>> stepsFrom (2, 1)
-- stepsFrom [(1,1),(2,0),(2,2),(3,1)]
stepsFrom :: Point -> [Point]
stepsFrom p = map (.+. p) directions

directions :: [Point]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- >>> (1, 1) .+. (2, 3)
-- (3,4)
(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

-- >>> toTopMap "12\n34"
-- fromList [((0,0),1),((0,1),3),((1,0),2),((1,1),4)]
toTopMap :: Input -> TopMap
toTopMap input = M.fromList [((x, y), digitToInt h) | (y, r) <- zip [0 ..] (lines input), (x, h) <- zip [0 ..] r]

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

example2 :: String
example2 = $(makeRelativeToProject "example2.txt" >>= embedStringFile)
