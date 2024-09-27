module Main where

import Data.Char
import Data.List
import Data.Map qualified as M
import Data.Maybe

example1 :: [String]
example1 =
  [ "broadcaster -> a, b, c",
    "%a -> b",
    "%b -> c",
    "%c -> inv",
    "&inv -> a"
  ]

example2 :: [String]
example2 =
  [ "broadcaster -> a",
    "%a -> inv, con",
    "&inv -> b",
    "%b -> con",
    "&con -> output"
  ]

data Powered = On | Off deriving (Show, Eq)

data Module
  = FlipFlop {name :: String, powerStatus :: Powered, destinations :: [String]}
  | Conjunction {name :: String, lastPulses :: M.Map String Pulse, destinations :: [String]}
  | Broadcaster {name :: String, destinations :: [String]}
  deriving (Show, Eq)

type System = M.Map String Module

data Pulse = High | Low deriving (Show, Eq)

data Packet = Packet {pulse :: Pulse, from :: String, to :: String} deriving (Show, Eq)

parseModule :: String -> Module
parseModule m
  | Just rest <- stripPrefix "broadcaster" m =
      let (_, d) = break isAsciiLower rest
          destinations = words $ filter (\c -> isAsciiLower c || isSpace c) d
       in Broadcaster "broadcaster" destinations
  | '%' : rest <- m =
      let (name, rest') = span isAsciiLower rest
          (_, d) = break isAsciiLower rest'
          destinations = words $ filter (\c -> isAsciiLower c || isSpace c) d
       in FlipFlop name Off destinations
  | '&' : rest <- m =
      let (name, rest') = span isAsciiLower rest
          (_, d) = break isAsciiLower rest'
          destinations = words $ filter (\c -> isAsciiLower c || isSpace c) d
       in Conjunction name M.empty destinations

findLastPulsesFor :: String -> [Module] -> M.Map String Pulse
findLastPulsesFor n = M.fromList . map (\m -> (name m, Low)) . filter (\m -> n `elem` destinations m)

parseModules :: String -> M.Map String Module
parseModules input =
  let modulesWithoutLastPulseList = map parseModule . lines $ input
      populateLastPulses c@Conjunction {name = n, destinations = d} = (n, Conjunction n (findLastPulsesFor n modulesWithoutLastPulseList) d)
      populateLastPulses m = (name m, m)
   in M.fromList . map populateLastPulses $ modulesWithoutLastPulseList

handle :: Module -> String -> Pulse -> (Module, [Packet])
handle b@Broadcaster {name = n, destinations = d} _ p = (b, map (Packet p n) d)
handle f@FlipFlop {} _ High = (f, [])
handle FlipFlop {name = n, destinations = d, powerStatus = power} _ Low =
  let power' = if power == On then Off else On
      pulse' = if power == On then Low else High
   in (FlipFlop n power' d, map (Packet pulse' n) d)
handle Conjunction {name = n, destinations = d, lastPulses = l} f p =
  let l' = M.insert f p l
      p' = if all (== High) $ M.elems l' then Low else High
   in (Conjunction n l' d, map (Packet p' n) d)

processOnce :: (System, [Packet], String, Bool) -> (System, [Packet], String, Bool)
processOnce (s, [], c, b) = (s, [], c, b)
processOnce (s, Packet {from = f, to = t, pulse = p} : xs, c, b) =
  if t `M.notMember` s
    then (s, xs, c, b)
    else
      let m = s M.! t
          (m', ps) = handle m f p
          b' = name m' == c && (all (== High) . M.elems . lastPulses $ m')
       in (M.insert t m' s, xs ++ ps, c, b || b')

cycleSystem :: System -> String -> (System, Bool)
cycleSystem s con = head . map (\(s', _, _, b) -> (s', b)) . dropWhile (\(_, ps, _, _) -> ps /= []) $ iterate processOnce (s, [Packet Low "button" "broadcaster"], con, False)

buttonPushes :: String -> System -> [Bool]
buttonPushes con s =
  let (s', b) = cycleSystem s con
   in b : buttonPushes con s'

solve :: String -> Int
solve input =
  let xm = (+) 1 . length . takeWhile not . buttonPushes "xm" . parseModules $ input
      vt = (+) 1 . length . takeWhile not . buttonPushes "vt" . parseModules $ input
      gr = (+) 1 . length . takeWhile not . buttonPushes "gr" . parseModules $ input
      dt = (+) 1 . length . takeWhile not . buttonPushes "dt" . parseModules $ input
   in lcm dt (lcm gr (lcm xm vt))

main = readFile "input.txt" >>= print . solve