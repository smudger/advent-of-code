module Main (main) where

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 input = length (lines input)

part2 :: String -> Int
part2 _input = 0
