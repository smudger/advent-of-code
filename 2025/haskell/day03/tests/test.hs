module Main (main) where

import Data.List (sort)
import Solver (Solution (part1, part2), parse, solve)
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
      {--
        Postconditions
      --}
      {--
        Metamorphic properties
      --}
      testProperty "sort == sort . reverse" prop_SortReverse
      {--
        Inductive properties
      --}
      {--
        Model-based properties
      --}
    ]

prop_SortReverse :: [Int] -> Bool
prop_SortReverse xs = sort xs == sort (reverse xs)

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
assert_Part1Example1 = (part1 . solve . parse $ example1) @?= 357

assert_Part2Example1 :: Assertion
assert_Part2Example1 = (part2 . solve . parse $ example1) @?= 3121910778619

{----------------------------------------------------------------------------------------------------------------------
    Examples
----------------------------------------------------------------------------------------------------------------------}

example1 :: String
example1 = "987654321111111\n811111111111119\n234234234234278\n818181911112111"
