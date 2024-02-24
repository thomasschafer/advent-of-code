module Day03 (part1, part2) where

data Position = Empty | Tree
  deriving (Eq)

traverseGrid :: Int -> Int -> Int -> [[Position]] -> Int
traverseGrid row col acc grid =
  if row >= length grid
    then acc
    else traverseGrid (row + 1) ((col + 3)  `mod` length (head grid)) updatedAcc grid
  where
    updatedAcc = acc + if (grid !! row !! col) == Tree then 1 else 0

part1 :: String -> Int
part1 = traverseGrid 0 0 0 . map (map parse) . lines
  where
    parse '#' = Tree
    parse '.' = Empty

part2 :: String -> Int
part2 = const 2