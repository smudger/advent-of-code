module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.List (group, subsequences)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve =
  let deltas report = map (\(a, b) -> b - a) $ zip report (drop 1 report)
      checkAllIncreasingOrDecreasing = (== 1) . length . group . map signum . deltas
      checkLevelDifference = all (\d -> d >= 1 && d <= 3) . map abs . deltas
      isSafe report = checkAllIncreasingOrDecreasing report && checkLevelDifference report
      dampen report = filter ((>= (length report) - 1) . length) . subsequences $ report
   in length . filter (any isSafe) . map (dampen . map (read @Int) . words) . lines

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)