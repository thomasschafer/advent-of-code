module Day14 (day14Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.List (maximumBy, minimumBy)
import Data.Maybe (fromMaybe)

incrementBy :: (Hashable k) => Int -> k -> HashMap k Int -> HashMap k Int
incrementBy amount char counts = HM.insert char (fromMaybe 0 (HM.lookup char counts) + amount) counts

compareOccurences :: (a, Int) -> (a, Int) -> Ordering
compareOccurences (_, freq1) (_, freq2)
  | freq1 < freq2 = LT
  | freq1 > freq2 = GT
  | otherwise = EQ

parsePolymerData :: String -> (String, HashMap (Char, Char) Char)
parsePolymerData polymerData = (template, mappings)
  where
    polymerLines = break (== "") $ lines polymerData
    template = head $ fst polymerLines
    parseLine l = (pair, insertionChar)
      where
        firstWord = head l
        pair = (head firstWord, firstWord !! 1)
        insertionChar = head (l !! 2)
    mappings = HM.fromList $ map (parseLine . words) $ tail $ snd polymerLines

applyPolymerRules :: Int -> String -> Int
applyPolymerRules numSteps polymerData = snd mostCommon - snd leastCommon
  where
    (template, mappings) = parsePolymerData polymerData
    pairs = zip (" " ++ template) (template ++ " ")
    pairCounts = foldl (flip (incrementBy 1)) HM.empty pairs

    updatedPairCounts = iterate growPolymer pairCounts !! numSteps

    growPolymer :: HashMap (Char, Char) Int -> HashMap (Char, Char) Int
    growPolymer = foldl addPairs HM.empty . HM.toList
      where
        addPairs :: HashMap (Char, Char) Int -> ((Char, Char), Int) -> HashMap (Char, Char) Int
        addPairs counts ((left, right), count) =
          case HM.lookup (left, right) mappings of
            Just newChar ->
              incrByCount (newChar, right) $
                incrByCount (left, newChar) counts
            Nothing -> incrByCount (left, right) counts
          where
            incrByCount = incrementBy count

    allCharCountList = HM.toList $ foldl updateCharCount HM.empty $ HM.toList updatedPairCounts
    validCharCountList = filter ((/= ' ') . fst) allCharCountList
    -- As we are counting pairs, we will have counted each char twice
    dedupedCharCountList = map (\(char, count) -> (char, count `div` 2)) validCharCountList

    updateCharCount :: HashMap Char Int -> ((Char, Char), Int) -> HashMap Char Int
    updateCharCount counts ((left, right), count) = incrByCount left $ incrByCount right counts
      where
        incrByCount = incrementBy count

    mostCommon = maximumBy compareOccurences dedupedCharCountList
    leastCommon = minimumBy compareOccurences dedupedCharCountList

solvePart1 :: String -> Int
solvePart1 = applyPolymerRules 10

solvePart2 :: String -> Int
solvePart2 = applyPolymerRules 40

day14Main :: IO ()
day14Main = do
  testData <- readFile "data/day_14_test.txt"
  realData <- readFile "data/day_14.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
