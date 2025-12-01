module Solver where

import Data.Maybe (mapMaybe)
import Prelude hiding (Left, Right)

{----------------------------------------------------------------------------------------------------------------------
    Solutions
----------------------------------------------------------------------------------------------------------------------}

part1 :: String -> Int
part1 = password1' . parse

part2 :: String -> Int
part2 = password2' . parse

{----------------------------------------------------------------------------------------------------------------------
    Helpers
----------------------------------------------------------------------------------------------------------------------}

newtype Position = Position Int deriving (Show)

data Rotation = Left Int | Right Int
  deriving (Show)

-- >>> rotate (Right 8) (Position 11)
-- Position 19
-- >>> rotate (Left 10) (Position 5)
-- Position 95
rotate :: Rotation -> Position -> Position
rotate (Left l) (Position p) = Position $ (p - l) `mod` 100
rotate (Right r) (Position p) = Position $ (p + r) `mod` 100

password1' :: [Rotation] -> Int
password1' = fst . foldl' processRotation (0, (Position 50))
  where
    processRotation :: (Int, Position) -> Rotation -> (Int, Position)
    processRotation (acc, pos) r =
      let acc' = if atOrigin pos' then acc + 1 else acc
          pos' = rotate r pos
       in (acc', pos')
      where
        atOrigin :: Position -> Bool
        atOrigin (Position 0) = True
        atOrigin _ = False

password2' :: [Rotation] -> Int
password2' = fst . foldl' processRotation (0, Position 50)
  where
    processRotation :: (Int, Position) -> Rotation -> (Int, Position)
    processRotation (acc, pos) r =
      let acc' = acc + countZeroes r pos
          pos' = rotate r pos
       in (acc', pos')
      where
        countZeroes :: Rotation -> Position -> Int
        countZeroes (Right dr) (Position p) = (p + dr) `div` 100
        countZeroes (Left dl) (Position 0) = dl `div` 100
        countZeroes (Left dl) (Position p) = (100 - p + dl) `div` 100

-- >>> parse "L68\nL30\nR48\n\n"
-- [Left 68,Left 30,Right 48]
parse :: String -> [Rotation]
parse = mapMaybe parseLine . lines
  where
    parseLine :: String -> Maybe Rotation
    parseLine ('L' : n) = Just $ Left $ read n
    parseLine ('R' : n) = Just $ Right $ read n
    parseLine _ = Nothing
