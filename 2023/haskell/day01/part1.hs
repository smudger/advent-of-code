import Data.Char

calibrateValue :: String -> Int
calibrateValue value =
  let digits = filter isDigit value
      firstDigit = digitToInt $ head digits
      lastDigit = digitToInt $ last digits
   in firstDigit * 10 + lastDigit

calibrateDocument :: String -> Int
calibrateDocument = sum . map calibrateValue . lines

main = do
  document <- readFile "input.txt"
  print $ calibrateDocument document