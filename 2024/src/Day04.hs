module Day04 (part1, part2) where

import Data.List (isPrefixOf, tails, transpose)

diags :: [[a]] -> [[a]]
diags xs =
  [ [ xs !! (startRow + i) !! (startCol + i)
      | i <- [0 .. max rows cols],
        startRow + i < rows,
        startCol + i < cols
    ]
    | (startRow, startCol) <- [(x, 0) | x <- [0 .. rows - 1]] ++ [(0, y) | y <- [1 .. cols - 1]]
  ]
  where
    rows = length xs
    cols = length $ head xs

perms :: [[a]] -> [[a]]
perms xs =
  xs
    ++ map reverse xs
    ++ transpose xs
    ++ map reverse (transpose xs)
    ++ diags xs
    ++ map reverse (diags xs)
    ++ diags (reverse xs)
    ++ map reverse (diags (reverse xs))

part1 :: String -> Int
part1 = length . concatMap (filter ("XMAS" `isPrefixOf`) . tails) . perms . lines

containsCrossMas :: [String] -> Bool
containsCrossMas xs =
  all
    (\(row, col, c) -> (xs !! row !! col) == c)
    [(0, 0, 'M'), (2, 0, 'M'), (1, 1, 'A'), (0, 2, 'S'), (2, 2, 'S')]

windows :: [[a]] -> [[[a]]]
windows xs = [window row col | row <- [0 .. length xs - 3], col <- [0 .. length (head xs) - 3]]
  where
    window row col = [[xs !! r !! c | c <- [col .. col + 2]] | r <- [row .. row + 2]]

orientations :: [[a]] -> [[[a]]]
orientations = take 4 . iterate rotate90
  where
    rotate90 = reverse . transpose

part2 :: String -> Int
part2 = length . concatMap (filter containsCrossMas . orientations) . windows . lines
