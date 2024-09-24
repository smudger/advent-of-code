{- cabal:
default-language: GHC2021
build-depends:
  base ^>=4.17.0.0,
  containers ^>=0.6.0.0,
  random ^>=1.0.0.0
-}

module Main where

import Data.Char (isAsciiLower)
import Data.Function
import Data.List
import Data.Map qualified as M
import System.Random

example :: [String]
example =
  [ "jqt: rhn xhk nvd",
    "rsh: frs pzl lsr",
    "xhk: hfx",
    "cmg: qnr nvd lhk bvb",
    "rhn: xhk bvb hfx",
    "bvb: xhk hfx",
    "pzl: lsr hfx nvd",
    "qnr: nvd",
    "ntq: jqt hfx bvb xhk",
    "nvd: lhk",
    "lsr: lhk",
    "rzs: qnr cmg lsr rsh",
    "frs: qnr lhk lsr"
  ]

type Graph = M.Map String [String]

adjacencyList :: [String] -> Graph
adjacencyList =
  let adjacencyListInRow row
        | (x : xs) <- labels = M.fromList $ (x, xs) : map (,[x]) xs
        where
          labels = filter (isAsciiLower . head) . groupBy ((==) `on` isAsciiLower) $ row
   in foldl (M.unionWith (++)) M.empty . map adjacencyListInRow

contractEdge :: String -> String -> Graph -> Graph
contractEdge a b g =
  let newLabel = a ++ "_" ++ b
      newEdges = concatMap (filter (\l -> l /= a && l /= b)) [g M.! a, g M.! b]
   in M.insert newLabel newEdges . M.map (map (\e -> if e == a || e == b then newLabel else e)) . M.filterWithKey (\k _ -> k /= a && k /= b) $ g

randomElement :: (RandomGen g) => g -> [String] -> (String, g)
randomElement g m
  | (i, g') <- randomR (0, length m - 1) g = (m !! i, g')

randomEdge :: (RandomGen g) => g -> Graph -> ((String, String), g)
randomEdge gen graph =
  let k = M.keys graph
      (a, gen') = randomElement gen k
      e = graph M.! a
      (b, gen'') = randomElement gen' e
   in ((a, b), gen'')

contract :: (RandomGen g) => g -> Graph -> (Graph, g)
contract gen graph
  | length graph == 2 = (graph, gen)
  | otherwise =
      let ((a, b), gen') = randomEdge gen graph
          graph' = contractEdge a b graph
       in contract gen' graph'

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

cutN :: (RandomGen g) => g -> Int -> Graph -> ([[String]], g)
cutN gen n graph
  | size <= n = (map (splitOn '_') . M.keys $ graph', gen')
  | otherwise = cutN gen' n graph
  where
    (graph', gen') = contract gen graph
    size = length . head . M.elems $ graph'

solve :: (RandomGen g) => g -> String -> (Int, g)
solve g input =
  let (cut, g') = cutN g 3 . adjacencyList . lines $ input
   in (length (head cut) * length (last cut), g')

main = do
  gen <- getStdGen
  readFile "input.txt" >>= print . fst . solve gen