module Day02 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (allEqual, chunksOf, toTuple)

solve :: (String -> [Int]) -> String -> Int
solve chunkLengths = sum . concatMap (filter (isInvalid . show) . parseRange) . splitOn ","
  where
    parseRange = uncurry enumFromTo . toTuple . map read . splitOn "-"
    isInvalid = any . repeats <*> chunkLengths
    repeats s n = length s `rem` n == 0 && (allEqual $ chunksOf n s)

part1 :: String -> Int
part1 = solve $ const [2]

part2 :: String -> Int
part2 = solve $ \s -> reverse [1 .. (length s `div` 2)]
