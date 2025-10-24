module Main (main) where

import Data.List (sort)
import Solver (part1, part2)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

{----------------------------------------------------------------------------------------------------------------------
    Main
----------------------------------------------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

{----------------------------------------------------------------------------------------------------------------------
    Property Tests
----------------------------------------------------------------------------------------------------------------------}

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ {--
        Invariants
      --}
      testProperty "All generated string inputs are valid" prop_ValidStringInput,
      testProperty "All generated model inputs are valid" prop_ValidModelInput,
      {--
        Postconditions
      --}
      testProperty "Part 1 solution is less than or equal to the sum of all numbers in the input" (prop_TotalSum part1),
      testProperty "Part 2 solution is less than or equal to the sum of all numbers in the input" (prop_TotalSum part2),
      {--
        Metamorphic properties
      --}
      testProperty "Part 1 solution increases or stays the same if you add another digit" (prop_AddDigit part1),
      testProperty "Part 2 solution increases or stays the same if you add another digit" (prop_AddDigit part2),
      testProperty "Part 1 solution decreases or stays the same if you add another newline" (prop_AddNewline part1),
      testProperty "Part 2 solution decreases or stays the same if you add another newline" (prop_AddNewline part2),
      {--
        Inductive properties
            Not possible in this case: We cannot easily define `part1 ('1' : "2\n3")`, given the solution to `part1 "2\n3"`.
            We can only do a metamorphic property e.g. propAddDigit.
      --}
      {--
        Model-based properties
            These aren't great examples of model-based properties as they partially re-implement the function under test.
            However, they do give us some security that our string parsing is correct.
      --}
      testProperty "Part 1 solution gives the same answer as our model" prop_ModelPart1,
      testProperty "Part 2 solution gives the same answer as our model" prop_ModelPart2
    ]

validChars :: [Char]
validChars = "\n0123456789"

newtype Input
  = Input String
  deriving (Show)

instance Arbitrary Input where
  arbitrary = Input <$> listOf (elements validChars)
  shrink (Input i) = Input <$> shrink i

newtype Model
  = Model [[Integer]]
  deriving (Show)

instance Arbitrary Model where
  arbitrary = Model <$> listOf (listOf (elements [0 .. 10000]))
  shrink (Model m) = Model <$> shrink m

valid :: String -> Bool
valid = all (`elem` validChars)

prop_ValidStringInput :: Input -> Bool
prop_ValidStringInput (Input s) = valid s

prop_ValidModelInput :: Model -> Bool
prop_ValidModelInput (Model m) = valid (toString m)

prop_TotalSum :: (String -> Integer) -> Input -> Bool
prop_TotalSum f (Input s) = f s <= totalSum s
  where
    totalSum = sum . map read . words . map (\c -> if c == '\n' then ' ' else c)

genAddDigit :: Gen (Input, Int, Char)
genAddDigit = do
  input <- arbitrary
  digit <- elements "0123456789"
  let n = (\(Input s) -> length s) input
  index <- choose (0, n)
  return (input, index, digit)

prop_AddDigit :: (String -> Integer) -> Property
prop_AddDigit f = forAll genAddDigit $ \((Input s), i, d) -> f s <= f (insertAt i d s)

genAddNewline :: Gen (Input, Int)
genAddNewline = do
  input <- arbitrary
  let n = (\(Input s) -> length s) input
  index <- choose (0, n)
  return (input, index)

prop_AddNewline :: (String -> Integer) -> Property
prop_AddNewline f = forAll genAddNewline $ \((Input s), i) -> f s >= f (insertAt i '\n' s)

insertAt :: Int -> a -> [a] -> [a]
insertAt _ x [] = [x]
insertAt 0 x ys = x : ys
insertAt n x (y : ys) = y : insertAt (n - 1) x ys

prop_ModelPart1 :: Model -> Bool
prop_ModelPart1 (Model m) = part1 (toString m) == modelResult m
  where
    modelResult xs = case map sum xs of
      [] -> 0
      xs' -> maximum xs'

prop_ModelPart2 :: Model -> Bool
prop_ModelPart2 (Model m) = part2 (toString m) == modelResult m
  where
    modelResult = sum . take 3 . reverse . sort . map sum

toString :: [[Integer]] -> String
toString = unlines . map (unlines . map show)

{----------------------------------------------------------------------------------------------------------------------
    Unit Tests
----------------------------------------------------------------------------------------------------------------------}

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "It solves part1 using example1" assert_Part1Example1,
      testCase "It solves part2 using example1" assert_Part2Example1
    ]

assert_Part1Example1 :: Assertion
assert_Part1Example1 = part1 example1 @?= 24000

assert_Part2Example1 :: Assertion
assert_Part2Example1 = part2 example1 @?= 45000

{----------------------------------------------------------------------------------------------------------------------
    Examples
----------------------------------------------------------------------------------------------------------------------}

example1 :: String
example1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
