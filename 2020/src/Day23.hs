module Day23 (part1, part2) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Utils (toInt)

move :: [Int] -> [Int]
move (current : cups) = tail newOrder ++ [head newOrder]
 where
  threeCups = take 3 cups
  rest = drop 3 cups
  destination = case filter (< current) rest of
    [] -> maximum rest
    cs -> maximum cs
  insertionIdx = fromJust (elemIndex destination rest) + 1
  newOrder = current : take insertionIdx rest ++ threeCups ++ drop insertionIdx rest
move cups = error $ "Not enough cups: " ++ show cups

part1 :: String -> String
part1 = concatMap show . orderAfter1 . (!! 100) . iterate move . map toInt
 where
  orderAfter1 cups = drop (idx + 1) cups ++ take idx cups
   where
    idx = fromJust $ elemIndex 1 cups

part2 :: String -> Int
part2 = const 2
