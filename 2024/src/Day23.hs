module Day23 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, maximumBy, nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (toTuple)

connections :: [(String, String)] -> HashMap String [String]
connections = foldl addNeighs HM.empty
  where
    addNeighs acc (a, b) = foldr (uncurry HM.insert) acc [(a, aNeighs), (b, bNeighs)]
      where
        aNeighs = b : fromMaybe [] (HM.lookup a acc)
        bNeighs = a : fromMaybe [] (HM.lookup b acc)

triangles :: HashMap String [String] -> [[String]]
triangles conns = nub . concatMap (map sort . triangles') . filter ((== 't') . head . fst) $ HM.toList conns
  where
    triangles' :: (String, [String]) -> [[String]]
    triangles' (node, neighs) =
      [ [node, neighs !! i, neighs !! j]
        | i <- [0 .. length neighs - 2],
          j <- [i + 1 .. length neighs - 1],
          maybe False ((neighs !! j) `elem`) $ HM.lookup (neighs !! i) conns
      ]

parse :: String -> [(String, String)]
parse = map (toTuple . splitOn "-") . lines

part1 :: String -> Int
part1 = length . triangles . connections . parse

largestClique :: HashMap String [String] -> Set String
largestClique conns = maximumBy (comparing length) $ bronKerbosch S.empty vertices S.empty
  where
    vertices = S.fromList $ HM.keys conns

    neighs v = maybe S.empty S.fromList $ HM.lookup v conns

    bronKerbosch potential candidates excluded =
      (if null candidates && null excluded then (potential :) else id) $
        bronKerbosch' (S.toList candidates) candidates excluded
      where
        bronKerbosch' [] _ _ = []
        bronKerbosch' (v : vs) cs ex =
          bronKerbosch (S.insert v potential) (cs `S.intersection` neighs v) (ex `S.intersection` neighs v)
            ++ bronKerbosch' vs (S.delete v cs) (S.insert v ex)

part2 :: String -> String
part2 = intercalate "," . sort . S.toList . largestClique . connections . parse
