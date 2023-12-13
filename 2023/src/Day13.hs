module Day13 (part1, part2) where

import Data.List (transpose)
import Data.List.Split (splitOn)

rowReflects :: Bool -> [String] -> Int -> Bool
rowReflects withSmudge d row = numMismatches == (if withSmudge then 1 else 0)
  where
    numMismatches =
      length . filter not . concat $
        zipWith (zipWith (==)) (reverse $ take row d) (drop row d)

colReflects :: Bool -> [String] -> Int -> Bool
colReflects withSmudge = rowReflects withSmudge . transpose

reflectionLine :: Bool -> [String] -> Int
reflectionLine withSmudge d = case (reflectedCols, reflectedRows) of
  ([c], []) -> c
  ([], [r]) -> 100 * r
  (_, _) -> error ("Unexpected match for (reflectedCols, reflectedRows): " ++ show (reflectedCols, reflectedRows))
  where
    reflectedRows = filter (rowReflects withSmudge d) [1 .. length d - 1]
    reflectedCols = filter (colReflects withSmudge d) [1 .. length (head d) - 1]

solve :: Bool -> String -> Int
solve withSmudge = sum . map (reflectionLine withSmudge . lines) . splitOn "\n\n"

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
