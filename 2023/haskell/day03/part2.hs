module Main where

import Data.Char
import Data.Function
import Data.List

example :: [String]
example =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

gears :: [String] -> [Gear]
gears = concatMap gearsInRow . zip [0 ..]

gearsInRow :: (Int, String) -> [Gear]
gearsInRow (y, row) = map (\((x, d) : ys) -> Gear (x, y) (read @Int (d : map snd ys))) . filter (isDigit . snd . head) . groupBy ((==) `on` (isDigit . snd)) . zip [0 ..] $ row

gearLocs :: Gear -> [Location]
gearLocs Gear {value = v, gear_loc = (x, y)} = map (,y) $ take (length $ show v) [x ..]

markers :: [String] -> [Marker]
markers = concatMap markersInRow . zip [0 ..]

markersInRow :: (Int, String) -> [Marker]
markersInRow (y, row) = foldl (\acc (x, c) -> if isDigit c || c == '.' then acc else Marker (x, y) c : acc) [] . zip [0 ..] $ row

markerBox :: Marker -> [Location]
markerBox Marker {symbol = _, marker_loc = (x, y)} =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

markedLocs :: [String] -> [Location]
markedLocs = concatMap markerBox . markers

isValidGear :: [Location] -> Gear -> Bool
isValidGear locs = any (`elem` locs) . gearLocs

markerGears :: [String] -> [[Gear]]
markerGears input_lines =
  let findGearsForMarker m = filter (isValidGear $ markerBox m) $ gears input_lines
   in filter ((==) 2 . length) . map findGearsForMarker . filter ((==) '*' . symbol) . markers $ input_lines

type Location = (Int, Int)

data Gear = Gear {gear_loc :: Location, value :: Int} deriving (Show)

data Marker = Marker {marker_loc :: Location, symbol :: Char} deriving (Show)

main = do
  readFile "input.txt" >>= print . sum . map (product . map value) . markerGears . lines
