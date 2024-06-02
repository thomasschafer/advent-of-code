module Day02 (part1, part2) where

import Data.List.Split (splitOn)

updateAt :: Int -> a -> [a] -> [a]
updateAt idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs

solve :: Int -> [Int] -> [Int]
solve curIdx opCodes = case drop curIdx opCodes of
  (99 : _) -> opCodes
  (opNum : val1 : val2 : writePos : _) ->
    solve (curIdx + 4) (updateAt writePos (op (opCodes !! val1) (opCodes !! val2)) opCodes)
   where
    op = case opNum of
      1 -> (+)
      2 -> (*)

part1 :: String -> Int
part1 = head . solve 0 . restoreProgram . map read . splitOn ","
 where
  restoreProgram = updateAt 2 2 . updateAt 1 12

part2 :: String -> Int
part2 = const 2
