module Day07 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

parseEdges :: String -> [(String, (Int, String))]
parseEdges s = case splitOn " bags contain" s of
  [_, " no other bags."] -> []
  [color, rest] -> map ((\(numStr : col) -> (color, (read numStr, unwords col))) . take 3 . words) $ splitOn ", " rest
  patt -> error $ "Unexpected pattern" ++ show patt

bagsFrom :: HashMap String [String] -> String -> Int
bagsFrom colourMap = subtract 1 . length . bagsFrom' S.empty
  where
    bagsFrom' visited cur
      | cur `elem` visited = visited
      | otherwise = foldl bagsFrom' (S.insert cur visited) . fromMaybe [] $ HM.lookup cur colourMap

part1 :: String -> Int
part1 = flip bagsFrom "shiny gold" . buildMap . map reverseDirection . concatMap parseEdges . lines
  where
    reverseDirection (from, (_, to)) = (to, from)
    updateMap m (from, to) = HM.insert from (to : fromMaybe [] (HM.lookup from m)) m
    buildMap = foldl updateMap HM.empty

bagsContained :: HashMap String [(Int, String)] -> String -> Int
bagsContained colourMap =
  sum
    . maybe [] (map (\(num, colour) -> num * (1 + bagsContained colourMap colour)))
    . flip HM.lookup colourMap

part2 :: String -> Int
part2 = flip bagsContained "shiny gold" . buildMap . concatMap parseEdges . lines
  where
    updateMap m (from, to) = HM.insert from (to : fromMaybe [] (HM.lookup from m)) m
    buildMap = foldl updateMap HM.empty
