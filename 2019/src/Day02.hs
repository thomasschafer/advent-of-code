module Day02 (part1, part2) where

import Control.Monad (join)
import Data.List.Split (splitOn)

updateAt :: Int -> a -> [a] -> [a]
updateAt idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs

solve :: [Int] -> [Int] -> [Int]
solve cur = \case
  (99 : _) -> cur
  (opNum : val1 : val2 : writePos : rest) ->
    solve (updateAt writePos (op (cur !! val1) (cur !! val2)) cur) rest
   where
    op = case opNum of
      1 -> (+)
      2 -> (*)

part1 :: String -> Int
part1 = head . join solve . restoreProgram . map read . splitOn ","
 where
  restoreProgram = updateAt 2 2 . updateAt 1 12

part2 :: String -> Int
part2 = const 2
