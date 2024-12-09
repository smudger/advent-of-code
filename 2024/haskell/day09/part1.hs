module Main where

import Data.Char (digitToInt)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)

main :: IO ()
main = do
  print . solve $ input
  where
    input = $(makeRelativeToProject "input.txt" >>= embedStringFile)

solve :: String -> Int
solve = checksum . defrag . toBlocks

data Content = File Int | Free deriving (Show)

type Block = (Content, Int)

type Input = String

-- >>> toBlocks "123"
-- [(File 0,1),(Free,2),(File 1,3)]
toBlocks :: String -> [Block]
toBlocks = map toBlock . zip [0 ..]

-- >>> toBlock (1, '6')
-- (Free,6)
-- >>> toBlock (2, '4')
-- (File 1,4)
toBlock :: (Int, Char) -> Block
toBlock (pos, c)
  | pos `mod` 2 == 0 = (File (pos `div` 2), digitToInt c)
  | otherwise = (Free, digitToInt c)

-- >>> defrag [(File 997, 2), (Free, 3), (File 998, 4), (File 999, 2), (Free, 2)]
-- [(File 997,2),(File 999,2),(File 998,1),(File 998,3)]
defrag :: [Block] -> [Block]
defrag [] = []
defrag (b@((File _), _) : bs) = b : defrag bs
defrag ((Free, n) : bs) = b' : defrag bs'
  where
    (b', bs') = plug n bs

-- >>> plug 3 [(File 998, 3), (File 999, 5), (Free, 4)]
-- ((File 999,3),[(File 998,3),(File 999,2)])
plug :: Int -> [Block] -> (Block, [Block])
plug n bs = case (pop $ revDropWhile isFree bs) of
  (bs', Just (File i, m)) -> spreadFile i n m bs'
  (bs', _) -> ((Free, n), bs')

-- >>> spreadFile 999 2 5 []
-- ((File 999,2),[(File 999,3)])
-- >>> spreadFile 999 5 2 []
-- ((File 999,2),[(Free,3)])
-- >>> spreadFile 999 2 2 []
-- ((File 999,2),[])
spreadFile :: Int -> Int -> Int -> [Block] -> (Block, [Block])
spreadFile i holeSize fileSize bs
  | holeSize == fileSize = ((File i, holeSize), bs)
  | holeSize > fileSize = ((File i, fileSize), (Free, holeSize - fileSize) : bs)
  | otherwise = ((File i, holeSize), bs ++ [(File i, fileSize - holeSize)])

-- >>> pop [1, 2, 3]
-- ([1,2],Just 3)
pop :: [a] -> ([a], Maybe a)
pop [] = ([], Nothing)
pop xs = (init xs, Just (last xs))

-- >>> revDropWhile (>3) [4, 1, 5, 4]
-- [4,1]
revDropWhile :: (a -> Bool) -> [a] -> [a]
revDropWhile f = reverse . dropWhile f . reverse

-- >>> isFree (Free, 4)
-- True
-- >>> isFree (File 4, 2)
-- False
isFree :: Block -> Bool
isFree (Free, _) = True
isFree _ = False

-- >>> checksum [(File 0, 2), (File 9, 2), (File 8, 1), (Free, 2)]
-- 77
checksum :: [Block] -> Int
checksum = go 0
  where
    go n ((File i, l) : bs) = ((* i) . sum . take l $ [n ..]) + go (n + l) bs
    go n ((Free, _) : bs) = go n bs
    go _ [] = 0

example :: String
example = $(makeRelativeToProject "example.txt" >>= embedStringFile)

input :: String
input = $(makeRelativeToProject "input.txt" >>= embedStringFile)
