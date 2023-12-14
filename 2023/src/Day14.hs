module Day14 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, sort, sortBy, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)

data Direction = North | West | South | East

rollRow :: Bool -> String -> String
rollRow toLeft = intercalate "#" . map (if toLeft then sortBy (comparing Down) else sort) . splitOn "#"

roll :: Direction -> [String] -> [String]
roll North = transpose . map (rollRow True) . transpose
roll West = map (rollRow True)
roll South = transpose . map (rollRow False) . transpose
roll East = map (rollRow False)

solve :: ([String] -> [String]) -> String -> Int
solve transform = sum . map rowLoad . transpose . transform . lines
  where
    rowLoad s = sum . map fst . filter ((== 'O') . snd) $ zip (reverse [1 .. length s]) s

part1 :: String -> Int
part1 = solve (roll North)

spinCycle :: HashMap [String] [Int] -> Int -> [String] -> [String]
spinCycle _ 0 s = s
spinCycle seen n s
  | n < 0 = error (show n)
  | otherwise = case HM.lookup s seen of
      Just (lastSeen : _) -> if jumpTo < n then spinCycle seen jumpTo s else cont
        where
          jumpTo = n `mod` (lastSeen - n)
      Just lastSeens -> error ("Unexpected last seen arr: " ++ show lastSeens)
      _ -> cont
  where
    updatedSeen = HM.insert s (n : fromMaybe [] (HM.lookup result seen)) seen
    result = roll East . roll South . roll West $ roll North s
    cont = spinCycle updatedSeen (n - 1) result

part2 :: String -> Int
part2 = solve (spinCycle HM.empty 1000000000)
