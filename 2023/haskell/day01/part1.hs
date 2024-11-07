module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Char

main :: IO ()
main = do
  print . solve $ input
  where input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> String
solve = show . sum . map calibrateValue . lines

calibrateValue :: String -> Int
calibrateValue value =
  let digits = filter isDigit value
      firstDigit = digitToInt $ head digits
      lastDigit = digitToInt $ last digits
   in firstDigit * 10 + lastDigit