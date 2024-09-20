import Data.Char
import Data.List
import Data.Maybe

digitAtStart :: String -> Maybe Int
digitAtStart haystack
  | [] <- haystack = Nothing
  | "one" `isPrefixOf` haystack = Just 1
  | "two" `isPrefixOf` haystack = Just 2
  | "three" `isPrefixOf` haystack = Just 3
  | "four" `isPrefixOf` haystack = Just 4
  | "five" `isPrefixOf` haystack = Just 5
  | "six" `isPrefixOf` haystack = Just 6
  | "seven" `isPrefixOf` haystack = Just 7
  | "eight" `isPrefixOf` haystack = Just 8
  | "nine" `isPrefixOf` haystack = Just 9
  | x : _ <- haystack = if isDigit x then Just $ digitToInt x else Nothing

calibrateValue :: String -> Int
calibrateValue value =
  let digits = mapMaybe digitAtStart . tails $ value
   in head digits * 10 + last digits

calibrateDocument :: String -> Int
calibrateDocument = sum . map calibrateValue . lines

main = do
  document <- readFile "input.txt"
  print $ calibrateDocument document