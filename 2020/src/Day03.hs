module Day03 (part1, part2) where

data Position = Empty | Tree
  deriving (Eq)

traverseGrid :: (Int, Int) -> [[Position]] -> Int
traverseGrid step@(rowStep, colStep) = traverseGrid' (0, 0) 0
  where
    traverseGrid' (row, col) acc grid =
      if row >= length grid
        then acc
        else traverseGrid' updatedCoords updatedAcc grid
      where
        updatedAcc = acc + if (grid !! row !! col) == Tree then 1 else 0
        updatedCoords = (row + rowStep, (col + colStep) `mod` length (head grid))

parse :: Char -> Position
parse '#' = Tree
parse '.' = Empty

part1 :: String -> Int
part1 = traverseGrid (1, 3) . map (map parse) . lines

part2 :: String -> Int
part2 =
  product
    . flip map [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    . flip traverseGrid
    . map (map parse)
    . lines
