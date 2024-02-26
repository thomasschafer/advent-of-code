module Day07 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)

parseEdges :: String -> [(String, String)]
parseEdges s = case splitOn " bags contain" s of
  [_, " no other bags."] -> []
  [color, rest] -> map ((\(_ : col) -> (color, unwords col)) . take 3 . words) $ splitOn ", " rest
  patt -> error $ "Unexpected pattern" ++ show patt

buildMap :: [(String, String)] -> HashMap String [String]
buildMap = foldl update HM.empty
  where
    update m (from, to) = HM.insert from (to : fromMaybe [] (HM.lookup from m)) m

bagsFrom :: HashMap String [String] -> String -> Int
bagsFrom colourMap = subtract 1 . length . bagsFrom' S.empty
  where
    bagsFrom' visited cur
      | cur `elem` visited = visited
      | otherwise = foldl bagsFrom' (S.insert cur visited) . fromMaybe [] $ HM.lookup cur colourMap

part1 :: String -> Int
part1 = flip bagsFrom "shiny gold" . buildMap . map swap . concatMap parseEdges . lines

part2 :: String -> Int
part2 = const 2
