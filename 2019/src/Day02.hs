module Day02 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (updateAt)

runProgram :: Int -> [Int] -> [Int]
runProgram curIdx opCodes = case drop curIdx opCodes of
  (99 : _) -> opCodes
  (opNum : val1 : val2 : writePos : _) ->
    runProgram (curIdx + 4) (updateAt writePos (op (opCodes !! val1) (opCodes !! val2)) opCodes)
    where
      op = case opNum of
        1 -> (+)
        2 -> (*)

solve :: (Int, Int) -> [Int] -> Int
solve (noun, verb) = head . runProgram 0 . restoreProgram
  where
    restoreProgram = updateAt 2 verb . updateAt 1 noun

parse :: (Read a) => String -> [a]
parse = map read . splitOn ","

part1 :: String -> Int
part1 = solve (12, 2) . parse

part2 :: String -> Int
part2 s =
  head
    [ 100 * noun + verb
      | noun <- [0 .. 99],
        verb <- [0 .. 99],
        solve (noun, verb) opCodes == 19690720
    ]
  where
    opCodes = parse s
