module Main where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe

example :: [String]
example =
  [ "19, 13, 30 @ -2,  1, -2",
    "18, 19, 22 @ -1, -1, -2",
    "20, 25, 34 @ -2, -2, -4",
    "12, 31, 28 @ -1, -2, -1",
    "20, 19, 15 @  1, -5, -3"
  ]

type Vector = (Double, Double, Double)

type PosVel = (Vector, Vector)

type Eq2 = (Double, Double)

parseRow :: String -> PosVel
parseRow row =
  let isDigitOrDash c = isDigit c || c == '-'
      nums = map (read @Double) . filter (isDigitOrDash . head) . groupBy ((==) `on` isDigitOrDash) $ row
      ([px, py, pz], [vx, vy, vz]) = splitAt 3 nums
   in ((px, py, pz), (vx, vy, vz))

posVelToEq2 :: PosVel -> Eq2
posVelToEq2 ((px, py, _), (vx, vy, _)) =
  let gradient = vy / vx
      intercept = py - gradient * px
   in (gradient, intercept)

pairs xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

futureIntersectionPoint2 :: (PosVel, PosVel) -> Maybe Vector
futureIntersectionPoint2 (pv1, pv2) =
  let ((a1, b1), (a2, b2)) = (posVelToEq2 pv1, posVelToEq2 pv2)
      isParallel = a1 == a2
   in if isParallel
        then Nothing
        else
          let x = (b2 - b1) / (a1 - a2)
              y = a1 * x + b1
              point = (x, y, 0)
              time1 = timesteps point pv1
              time2 = timesteps point pv2
           in if (time1 >= 0) && (time2 >= 0) then Just point else Nothing

timesteps :: Vector -> PosVel -> Double
timesteps (tx, _, _) ((px, _, _), (vx, _, _)) = (tx - px) / vx

isInBox2 :: Double -> Double -> Vector -> Bool
isInBox2 min max (x, y, _) = and [x >= min, x <= max, y >= min, y <= max]

main = do
  readFile "input.txt" >>= print . length . filter (isInBox2 200000000000000 400000000000000) . mapMaybe futureIntersectionPoint2 . pairs . map parseRow . lines
