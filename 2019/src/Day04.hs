module Day04 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (toTuple)

data NumMatches = All | Any

digitsSatisfy :: NumMatches -> (Char -> Char -> Bool) -> Int -> Bool
digitsSatisfy numMatches p x =
  (case numMatches of All -> all; Any -> any) (uncurry p) $
    zip s (tail s)
  where
    s = show x

hasAdjacentDuplicates :: Int -> Bool
hasAdjacentDuplicates = digitsSatisfy Any (==)

isIncreasing :: Int -> Bool
isIncreasing = digitsSatisfy All (\a b -> toInt a <= toInt b)
  where
    toInt x = read [x] :: Int

-- Returns result in reverse order
runLengths :: String -> [Int]
runLengths [] = []
runLengths (char : chars) = runLengths' chars (char, 1) []
  where
    runLengths' [] (_, count) acc = count : acc
    runLengths' (cur : rest) (prev, count) acc
      | cur == prev = runLengths' rest (cur, count + 1) acc
      | otherwise = runLengths' rest (cur, 1) (count : acc)

passwordsInRange :: [Int -> Bool] -> Int -> Int -> Int
passwordsInRange preds lower upper = length $ filter isValid [lower .. upper]
  where
    isValid x = all ($ x) preds

hasAdjacentGroupOfLen :: Int -> Int -> Bool
hasAdjacentGroupOfLen len num = len `elem` runLengths (show num)

solve :: [Int -> Bool] -> String -> Int
solve preds = uncurry (passwordsInRange preds) . toTuple . map read . splitOn "-"

part1 :: String -> Int
part1 = solve [hasAdjacentDuplicates, isIncreasing]

part2 :: String -> Int
part2 = solve [hasAdjacentGroupOfLen 2, isIncreasing]
