module Day3 (day3Main) where

import Data.List (group, sort)
import Utils (binaryStringToInt)

charCounts :: (Ord b) => [b] -> [(Int, b)]
charCounts ns = [(length ks, head ks) | ks <- group (sort ns)]

groupChars :: [String] -> [[(Int, Char)]]
groupChars xs =
  let nonEmptyStrs = filter (not . null) xs
   in if null nonEmptyStrs
        then []
        else charCounts (map head nonEmptyStrs) : groupChars (map tail nonEmptyStrs)

solvePart1 :: [String] -> Int
solvePart1 diagnosticData = do
  gammaRate * epsilonRate
  where
    groupedChars = groupChars diagnosticData
    mostFrequentChars = map (snd . maximum) groupedChars
    gammaRate = binaryStringToInt mostFrequentChars
    leastFrequentChars = map (\x -> if x == '1' then '0' else '1') mostFrequentChars
    epsilonRate = binaryStringToInt leastFrequentChars

filterByFreq :: ([(Int, Char)] -> Char) -> [String] -> Int -> String
filterByFreq filterFunc diagnosticData curIdx =
  if length filteredData <= 1
    then head filteredData
    else filterByFreq filterFunc filteredData (curIdx + 1)
  where
    counts = charCounts $ map (!! curIdx) diagnosticData
    requiredChar = filterFunc counts
    filteredData = filter ((== requiredChar) . (!! curIdx)) diagnosticData

calcOxygenGenRating :: [String] -> Int
calcOxygenGenRating diagnosticData =
  binaryStringToInt $ filterOxygenGenRating diagnosticData 0
  where
    mostCommon counts = case counts of
      [(_, a), (_, b)] | a == b -> '1'
      _ -> snd $ maximum counts
    filterOxygenGenRating = filterByFreq mostCommon

calcCo2ScrubRating :: [String] -> Int
calcCo2ScrubRating diagnosticData = binaryStringToInt $ filterCo2ScrubRating diagnosticData 0
  where
    leastCommon counts = case counts of
      [(_, a), (_, b)] | a == b -> '0'
      _ -> snd $ minimum counts
    filterCo2ScrubRating = filterByFreq leastCommon

solvePart2 :: [String] -> Int
solvePart2 diagnosticData =
  oxygenGenRating * co2ScrubRating
  where
    oxygenGenRating = calcOxygenGenRating diagnosticData
    co2ScrubRating = calcCo2ScrubRating diagnosticData

solveAndPrint :: String -> IO ()
solveAndPrint filePath = do
  diagnosticData <- readFile filePath
  print $ solvePart1 $ lines diagnosticData
  print $ solvePart2 $ lines diagnosticData

day3Main :: IO ()
day3Main = do
  solveAndPrint "data/day_3_test.txt"
  solveAndPrint "data/day_3.txt"
