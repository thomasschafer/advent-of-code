module Day11 (part1, part2) where

import Control.Arrow (first, second)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Utils (toTuple, (...))

data Status = InProgress | Visited {numPaths :: Int}

countPaths :: String -> String -> HashMap String [String] -> Int
countPaths start end connections = fst $ go HM.empty start
  where
    go visited cur
      | cur == end = (1, visited)
      | otherwise = case HM.lookup cur visited of
          Just InProgress -> (0, visited)
          Just (Visited {numPaths}) -> (numPaths, visited)
          Nothing -> (n, HM.insert cur (Visited {numPaths = n}) newVisited)
            where
              (n, newVisited) =
                foldl (uncurry update) (0, HM.insert cur InProgress visited)
                  . fromMaybe []
                  $ HM.lookup cur connections
              update count = first (count +) ... go

parse :: String -> HashMap String [String]
parse = HM.fromList . map (second words . toTuple . splitOn ": ") . lines

part1 :: String -> Int
part1 = countPaths "you" "out" . parse

part2 :: String -> Int
part2 s = (paths "svr" "dac" * paths "dac" "fft" * paths "fft" "out") + (paths "svr" "fft" * paths "fft" "dac" * paths "dac" "out")
  where
    connections = parse s
    paths start end = countPaths start end connections
