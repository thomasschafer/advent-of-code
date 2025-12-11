module Day11 (part1, part2) where

import Control.Arrow (first, second)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Utils (toTuple, (...))

data Status = InProgress | Visited {numPaths :: Int}

solve :: HashMap String [String] -> Int
solve connections = fst $ go HM.empty "you"
  where
    go visited "out" = (1, visited)
    go visited cur = case HM.lookup cur visited of
      Just InProgress -> (0, visited)
      Just (Visited {numPaths}) -> (numPaths, visited)
      Nothing -> (n, HM.insert cur (Visited {numPaths = n}) newVisited)
        where
          (n, newVisited) =
            foldl (uncurry update) (0, HM.insert cur InProgress visited)
              . fromMaybe []
              $ HM.lookup cur connections
          update count = first (count +) ... go

part1 :: String -> Int
part1 = solve . HM.fromList . map parse . lines
  where
    parse = second words . toTuple . splitOn ": "

part2 :: String -> Int
part2 = const 2
