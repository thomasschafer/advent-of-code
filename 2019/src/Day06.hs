module Day06 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Utils (toTuple)

addOrbit :: String -> String -> HashMap String [String] -> HashMap String [String]
addOrbit from to graph = HM.insert from (to : fromMaybe [] (HM.lookup from graph)) graph

parseOrbits :: Bool -> String -> HashMap String [String]
parseOrbits symmetric = foldl buildGraph HM.empty . map (toTuple . splitOn ")") . lines
  where
    buildGraph graph (from, to) =
      ( (if symmetric then addOrbit to from else id)
          . addOrbit from to
      )
        graph

orbitsFrom :: String -> HashMap String [String] -> (Int, Int) -- (numOrbits, numObjects)
orbitsFrom cur graph =
  ( sum (map (uncurry (+)) subOrbits), -- Add up number of sub orbits, and add one new orbit per object
    1 + sum (map snd subOrbits)
  )
  where
    subOrbits = maybe [] (map (`orbitsFrom` graph)) $ HM.lookup cur graph

part1 :: String -> Int
part1 = fst . orbitsFrom "COM" . parseOrbits False

minDist :: String -> String -> HashMap String [String] -> Int
minDist fromInitial to graph = minDist' (S.singleton fromInitial) 0 S.empty
  where
    minDist' from acc seen
      | to `elem` from = acc
      | otherwise = minDist' next (acc + 1) from
      where
        updateNext x = S.union . maybe S.empty (S.fromList . filter (`notElem` seen)) $ HM.lookup x graph
        next = foldr updateNext S.empty from

part2 :: String -> Int
part2 = subtract 2 . minDist "YOU" "SAN" . parseOrbits True
