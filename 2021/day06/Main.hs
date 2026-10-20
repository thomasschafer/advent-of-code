module Main (main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Aoc (splitBy)

updateCountsNextDay :: HashMap Int Int -> HashMap Int Int
updateCountsNextDay prevCounts = foldl fishUpdate HM.empty (HM.toList prevCounts)
  where
    fishUpdate :: HashMap Int Int -> (Int, Int) -> HashMap Int Int
    fishUpdate newCounts (age, parentCount) = countsWithChildren
      where
        newAge = if age == 0 then 6 else age - 1
        countsWithParents = HM.insertWith (+) newAge parentCount newCounts
        countsWithChildren = if age == 0 then HM.insertWith (+) 8 parentCount countsWithParents else countsWithParents

simulateDays :: Int -> String -> Int
simulateDays numDays ageData = numFish
  where
    ages = map read $ splitBy ',' ageData :: [Int]

    addAge counts age = HM.insert age updatedCount counts
      where
        updatedCount = fromMaybe 0 (HM.lookup age counts) + 1

    agesInitial = foldl addAge HM.empty ages
    agesAfterNDays = iterate updateCountsNextDay agesInitial !! numDays
    numFish = sum $ map snd $ HM.toList agesAfterNDays

main :: IO ()
main = do
  realData <- getContents
  print $ simulateDays 80 realData
  print $ simulateDays 256 realData
