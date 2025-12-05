module Main (main) where

import Data.List (sort)
import Solver (parse, part1, part2)
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
assert_Part1Example1 = ((uncurry part1) . parse $ example1) @?= 3

assert_Part2Example1 :: Assertion
assert_Part2Example1 = (part2 . fst . parse $ example1) @?= 14

{----------------------------------------------------------------------------------------------------------------------
    Examples
----------------------------------------------------------------------------------------------------------------------}

example1 :: String
example1 = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
