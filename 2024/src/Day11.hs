module Day11 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Utils (freqCounts, mapTuple, toList, (...))

addToVal :: (Hashable a, Num b) => a -> b -> HashMap a b -> HashMap a b
addToVal key val h = flip (HM.insert key) h . (val +) . fromMaybe 0 $ HM.lookup key h

blink :: HashMap Int Int -> HashMap Int Int
blink = flip HM.foldlWithKey HM.empty $ \acc num count ->
  let s = show num
      newVals
        | num == 0 = [1]
        | even (length s) = toList . mapTuple read $ splitAt (length s `div` 2) s
        | otherwise = [num * 2024]
   in foldr (uncurry addToVal . (,count)) acc newVals

part1, part2 :: String -> Int
(part1, part2) = mapTuple (flip solve) (25, 75)
  where
    solve = sum ... (!!) . iterate blink . freqCounts . map read . words
