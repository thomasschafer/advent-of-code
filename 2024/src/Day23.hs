module Day23 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
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

part1 :: String -> Int
part1 = length . triangles . connections . map (toTuple . splitOn "-") . lines

part2 :: String -> Int
part2 = const 2
