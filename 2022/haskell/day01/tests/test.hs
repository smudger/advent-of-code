module Main (main) where

import Data.List
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
    [ testProperty "sort == sort . reverse" prop_SortSortReverse,
      testProperty "Fermat's little theorem" prop_FermatsLittleTheorem
    ]

prop_SortSortReverse :: [Int] -> Property
prop_SortSortReverse xs = sort xs === sort (reverse xs)

prop_FermatsLittleTheorem :: Integer -> Property
prop_FermatsLittleTheorem x = (x ^ 7 - x) `mod` 7 === 0

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
