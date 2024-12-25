module Main where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = print . solve $ input

solve :: String -> String
solve x = x

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
