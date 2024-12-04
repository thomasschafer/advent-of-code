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

part2 :: String -> Int
part2 = const 2
