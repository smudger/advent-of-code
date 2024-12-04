module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> String
solve x = x

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)
