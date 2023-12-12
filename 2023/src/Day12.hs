module Day12 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate)
import Data.List.Split (splitOn)

type SpringRecord = (String, [Int])

parseLine :: String -> SpringRecord
parseLine = (\[springs, groupNums] -> (springs, map read (splitOn "," groupNums))) . splitOn " "

type SpringMemo = HashMap SpringRecord Int

combine :: (Int, SpringMemo) -> (Int, SpringMemo) -> (Int, SpringMemo)
combine (r1, m1) (r2, m2) = (r1 + r2, HM.union m1 m2)

updateMemo :: SpringRecord -> (Int, SpringMemo) -> (Int, SpringMemo)
updateMemo springRec (res, memo) = (res, HM.insert springRec res memo)

possibleArrangements :: SpringRecord -> Int
possibleArrangements = fst . possibleArrangements' HM.empty
  where
    possibleArrangements' memo sr@(springs, []) =
      case HM.lookup sr memo of
        Just v -> (v, memo)
        Nothing -> updateMemo sr (if '#' `elem` springs then 0 else 1, memo)
    possibleArrangements' memo ([], _) = (0, memo)
    possibleArrangements' memo sr@(springs, groupNums) =
      case HM.lookup sr memo of
        Just v -> (v, memo)
        Nothing -> updateMemo sr result
      where
        (possibleBlock, rest) = splitAt (head groupNums) springs
        canConsume =
          all (`elem` ['?', '#']) possibleBlock
            && take 1 (drop (head groupNums) springs) /= ['#']
        consumedRes = if canConsume then possibleArrangements' memo (drop 1 rest, tail groupNums) else (0, memo)
        nonConsumedRes = possibleArrangements' (snd consumedRes) (tail springs, groupNums)
        result =
          if head groupNums > length springs
            then (0, memo)
            else case head springs of
              '#' -> consumedRes
              '?' -> combine consumedRes nonConsumedRes
              _ -> possibleArrangements' memo (tail springs, groupNums)

solve :: (SpringRecord -> SpringRecord) -> String -> Int
solve inputTransformation = sum . map (possibleArrangements . inputTransformation . parseLine) . lines

part1 :: String -> Int
part1 = solve id

unfold :: SpringRecord -> SpringRecord
unfold (springs, groupNums) = (intercalate "?" $ replicate 5 springs, concat $ replicate 5 groupNums)

part2 :: String -> Int
part2 = solve unfold
