module Main where

import Data.Char (digitToInt)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: Input -> Int
solve input = sum . map length . map (trailsToSummit tmap) . M.keys $ M.filter (== 0) tmap
  where
    tmap = toTopMap input

type Input = String

type Point = (Int, Int)

type TopMap = Map Point Int

type Trail = [Point]

trailsToSummit :: TopMap -> Point -> [Trail]
trailsToSummit tmap start = trailsToSummit' tmap (S.singleton [start]) S.empty

trailsToSummit' :: TopMap -> Set Trail -> Set Trail -> [Trail]
trailsToSummit' tmap inbox outbox
  | S.null inbox = S.toList outbox
  | isAtSummit tmap (S.elemAt 0 inbox) = trailsToSummit' tmap (S.drop 1 inbox) (S.insert (S.elemAt 0 inbox) outbox)
  | otherwise =
      let t = S.elemAt 0 inbox
          ts = S.delete t inbox
          currHeight = fromJust $ height tmap (head t)
          next = S.filter (atHeight tmap (currHeight + 1)) $ trailsFrom t
       in trailsToSummit' tmap (S.union next ts) outbox

isAtSummit :: TopMap -> Trail -> Bool
isAtSummit tmap t = atHeight tmap 9 t

-- >>> atHeight (M.fromList [((1, 1), 4)]) 4 (1, 1)
-- True
-- >>> atHeight (M.fromList [((1, 1), 4)]) 3 (1, 1)
-- False
-- >>> atHeight (M.fromList [((1, 1), 4)]) 3 (0, 0)
-- False
atHeight :: TopMap -> Int -> Trail -> Bool
atHeight tmap expected (p : _) = case height tmap p of
  Just h -> h == expected
  Nothing -> False
atHeight _ _ _ = False

-- >>> height (M.fromList [((1, 1), 4)]) (1, 1)
-- 4
height :: TopMap -> Point -> Maybe Int
height tmap p = M.lookup p tmap

-- >>> inMap (M.fromList [(1, 3)]) 2
-- False
-- >>> inMap (M.fromList [(1, 3)]) 1
-- True
inMap :: (Ord k) => Map k v -> k -> Bool
inMap xs x = M.member x xs

-- >>> trailsFrom [(2, 1)]
-- fromList [[(1,1),(2,1)],[(2,0),(2,1)],[(2,2),(2,1)],[(3,1),(2,1)]]
trailsFrom :: [Point] -> Set [Point]
trailsFrom t@(x : _) = S.map (\d -> ((x .+. d) : t)) directions

directions :: Set Point
directions = S.fromList [(1, 0), (-1, 0), (0, 1), (0, -1)]

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
