module Day04 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils (splitBy, withIdx)

parseNumMatches :: String -> Int
parseNumMatches line = length (S.intersection winningNums chosenNums)
  where
    nums = last $ splitBy ':' line
    [winningNums, chosenNums] =
      map (S.fromList . map read . splitBy ' ') $ splitBy '|' nums :: [Set Int]

part1 :: String -> Int
part1 = sum . map (cardsToPoints . parseNumMatches) . lines
  where
    cardsToPoints n = if n == 0 then 0 else 2 ^ (n - 1)

part2 :: String -> Int
part2 s = sum $ foldl update initial (withIdx numMatches)
  where
    numMatches = map parseNumMatches $ lines s
    initial = replicate (length numMatches) 1
    update acc (idx, num) =
      [ if idx < i && i <= (idx + num) then x + (acc !! idx) else x
        | (i, x) <- withIdx acc
      ]
