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
trailsToSummit tmap start = go (S.singleton [start]) S.empty
  where
    go inbox outbox
      | S.null inbox = S.toList outbox
      | (t, ts) <- (S.elemAt 0 inbox, S.drop 1 inbox) =
          if isAtSummit tmap t
            then go ts (S.insert t outbox)
            else
              let currHeight = fromJust $ trailHeight tmap t
                  next = S.filter (isAtHeight tmap (currHeight + 1)) $ trailsFrom t
               in go (S.union next ts) outbox

-- >>> isAtSummit (M.fromList [((1, 1), 9)]) [(1, 1)]
-- True
isAtSummit :: TopMap -> Trail -> Bool
isAtSummit tmap t = isAtHeight tmap 9 t

-- >>> isAtHeight (M.fromList [((1, 1), 4)]) 4 [(1, 1)]
-- True
isAtHeight :: TopMap -> Int -> Trail -> Bool
isAtHeight tmap expected t = case trailHeight tmap t of
  Just h -> h == expected
  Nothing -> False

-- >>> trailHeight (M.fromList [((1, 1), 4)]) [(1, 1)]
-- Just 4
trailHeight :: TopMap -> Trail -> Maybe Int
trailHeight tmap (p : _) = pointHeight tmap p
trailHeight _ _ = Nothing

-- >>> pointHeight (M.fromList [((1, 1), 4)]) (1, 1)
-- Just 4
pointHeight :: TopMap -> Point -> Maybe Int
pointHeight tmap p = M.lookup p tmap

-- >>> inMap (M.fromList [(1, 3)]) 2
-- False
-- >>> inMap (M.fromList [(1, 3)]) 1
-- True
inMap :: (Ord k) => Map k v -> k -> Bool
inMap xs x = M.member x xs

-- >>> trailsFrom [(2, 1)]
-- fromList [[(1,1),(2,1)],[(2,0),(2,1)],[(2,2),(2,1)],[(3,1),(2,1)]]
trailsFrom :: Trail -> Set Trail
trailsFrom t@(x : _) = S.map (\d -> ((x .+. d) : t)) directions
trailsFrom _ = S.empty

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
