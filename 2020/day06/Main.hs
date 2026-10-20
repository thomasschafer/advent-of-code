module Main (main) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

solve :: (Set Char -> Set Char -> Set Char) -> String -> Int
solve f = sum . map (length . foldl1 f . map S.fromList . lines) . splitOn "\n\n"

part1 :: String -> Int
part1 = solve S.union

part2 :: String -> Int
part2 = solve S.intersection
