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
solve input = sum . map length . map (filter (atHeight tmap 9)) . map (accessiblePoints tmap) . M.keys $ M.filter (== 0) tmap
  where
    tmap = toTopMap input

type Input = String

type Point = (Int, Int)

type TopMap = Map Point Int

accessiblePoints :: TopMap -> Point -> [Point]
accessiblePoints tmap start = accessiblePoints' tmap (S.singleton start) S.empty

accessiblePoints' :: TopMap -> Set Point -> Set Point -> [Point]
accessiblePoints' tmap inbox outbox =
  if S.null inbox
    then S.toList outbox
    else
      let p = S.elemAt 0 inbox
          ps = S.delete p inbox
          currHeight = fromJust $ height tmap p
          next = S.filter (atHeight tmap (currHeight + 1)) . S.filter (not . inSet outbox) $ pointsFrom p
       in accessiblePoints' tmap (S.union next ps) (S.insert p outbox)

-- >>> atHeight (M.fromList [((1, 1), 4)]) 4 (1, 1)
-- True
-- >>> atHeight (M.fromList [((1, 1), 4)]) 3 (1, 1)
-- False
-- >>> atHeight (M.fromList [((1, 1), 4)]) 3 (0, 0)
-- False
atHeight :: TopMap -> Int -> Point -> Bool
atHeight tmap expected p = case height tmap p of
  Just h -> h == expected
  Nothing -> False

-- >>> height (M.fromList [((1, 1), 4)]) (1, 1)
-- 4
height :: TopMap -> Point -> Maybe Int
height tmap p = M.lookup p tmap

-- >>> inSet (S.singleton 3) 2
-- False
-- >>> inSet (S.singleton 3) 3
-- True
inSet :: (Ord a) => Set a -> a -> Bool
inSet xs x = S.member x xs

-- >>> inMap (M.fromList [(1, 3)]) 2
-- False
-- >>> inMap (M.fromList [(1, 3)]) 1
-- True
inMap :: (Ord k) => Map k v -> k -> Bool
inMap xs x = M.member x xs

-- >>> pointsFrom (2, 1)
-- fromList [(1,1),(2,0),(2,2),(3,1)]
pointsFrom :: Point -> Set Point
pointsFrom p = S.map (.+. p) directions

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
