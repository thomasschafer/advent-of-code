module Day25 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Debug.Trace (trace)

parse :: String -> HashMap String (Set String)
parse = foldl udpateMap HM.empty . map parseLine . lines
  where
    parseLine = (\[name, connectedTo] -> (name, words connectedTo)) . splitOn ": "
    udpateMap acc (name, connectedTo) =
      foldl addNeighbors acc $
        (name, connectedTo) : map (,[name]) connectedTo
    addNeighbors acc (from, to) = HM.insert from updatedTo acc
      where
        updatedTo = foldr S.insert (fromMaybe S.empty $ HM.lookup from acc) to

groupSizes :: HashMap String (Set String) -> [(String, String)] -> [Int]
groupSizes mapping ws = map length groups
  where
    filteredMapping = foldl updateMapping HM.empty (HM.toList mapping)
      where
        updateMapping acc (name, tos) = HM.insert name updatedTos acc
          where
            toCheck = ws ++ map swap ws
            updatedTos = S.filter (`notElem` map snd (filter ((== name) . fst) toCheck)) tos

    groups = foldl updateGroups [] (HM.toList filteredMapping)
      where
        updateGroups acc (name, tos) = foldl1 S.union connectedGroups : notConnectedGroups
          where
            newGroup = S.insert name tos
            overlaps group = not . null $ group `S.intersection` newGroup
            connectedGroups = newGroup : filter overlaps acc
            notConnectedGroups = filter (not . overlaps) acc

doubles :: [a] -> [[a]]
doubles xs =
  [ [xs !! i, xs !! j]
    | j <- [1 .. length xs - 1],
      i <- [0 .. j - 1]
  ]

triples :: [a] -> [[a]]
triples xs =
  [ [xs !! i, xs !! j, xs !! k]
    | k <- [2 .. length xs - 1],
      j <- [1 .. k - 1],
      i <- [0 .. j - 1]
  ]

disconnectedGroupSizes :: HashMap String (Set String) -> Int
disconnectedGroupSizes mapping = case triplesToRemove of
  [[l1, l2]] -> l1 * l2
  ts -> error ("Expected single matching triplet, instead found:\n\n" ++ intercalate "\n\n" (map show ts) ++ "\n")
  where
    wires = [(c1, c2) | (c1, v) <- HM.toList mapping, c2 <- S.toList v, c1 < c2]
    atMost2Groups = (<= 2) . length . groupSizes mapping
    singlesToRemove =
      (\r -> trace ("singlesToRemove: " ++ show (length r)) r) $
        filter (atMost2Groups . (: [])) wires
    doublesToRemove =
      (\r -> trace ("doublesToRemove: " ++ show (length r)) r) $
        filter atMost2Groups $
          doubles singlesToRemove
    -- todo don't recreate triples, build from doubles
    triplesToRemove =
      (\r -> trace ("triplesToRemove: " ++ show (length r)) r) $
        filter ((== 2) . length) . map (groupSizes mapping) . triples . nub $
          concat doublesToRemove

part1 :: String -> Int
part1 = disconnectedGroupSizes . parse

part2 :: String -> Int
part2 = const 1