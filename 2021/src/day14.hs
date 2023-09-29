module Day14 (day14Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (maximumBy, minimumBy)
import Data.Maybe (fromJust, fromMaybe)

updateCounts :: HashMap Char Int -> Char -> HashMap Char Int
updateCounts counts char = HM.insert char (fromMaybe 0 (HM.lookup char counts) + 1) counts

compareOccurences :: (Char, Int) -> (Char, Int) -> Ordering
compareOccurences (_, freq1) (_, freq2)
  | freq1 < freq2 = LT
  | freq1 > freq2 = GT
  | otherwise = EQ

solvePart1 :: String -> Int
solvePart1 polymerData = snd mostCommon - snd leastCommon
  where
    polymerLines = break (== "") $ lines polymerData
    template = head $ fst polymerLines
    mappings = HM.fromList $ map ((\l -> (head l, l !! 2)) . words) $ tail $ snd polymerLines

    growPolymer :: String -> String
    growPolymer polymer = concatMap insertMiddleElements pairs -- todo
      where
        pairs = zip polymer (tail polymer ++ [' '])

        insertMiddleElements (c1, ' ') = [c1]
        insertMiddleElements (c1, c2) = c1 : fromJust (HM.lookup [c1, c2] mappings)

    finalPolymer = iterate growPolymer template !! 10
    occurences = HM.toList $ foldl updateCounts HM.empty finalPolymer
    mostCommon = maximumBy compareOccurences occurences
    leastCommon = minimumBy compareOccurences occurences

day14Main :: IO ()
day14Main = do
  testData <- readFile "data/day_14_test.txt"
  realData <- readFile "data/day_14.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData

-- print $ solvePart2 testData
-- print $ solvePart2 realData
