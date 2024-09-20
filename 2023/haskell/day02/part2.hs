module Main where

import Data.Char
import Data.List

example :: [String]
example =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

colorCount :: String -> [String] -> Int
colorCount _ [] = 0
colorCount c (x : xs) =
  let isThisColor = c `isInfixOf` x
      thisCount = read @Int . filter isDigit $ x
   in max (if isThisColor then thisCount else 0) (colorCount c xs)

parseRounds :: String -> [String]
parseRounds = concatMap (splitOn ',') . splitOn ';' . drop 2 . dropWhile (/= ':')

colorCounts :: String -> (Int, Int, Int)
colorCounts s =
  let r = parseRounds s
      red = colorCount "red" r
      green = colorCount "green" r
      blue = colorCount "blue" r
   in (red, green, blue)

main = do
  input <- readFile "input.txt"
  print . sum . map ((\(r, g, b) -> r * g * b) . colorCounts) . lines $ input
