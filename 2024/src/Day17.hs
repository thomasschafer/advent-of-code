module Day17 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits (Bits (xor))
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Utils (to3Tuple, toTuple)

parse :: String -> ((Int, Int, Int), [Int])
parse = bimap registers program . toTuple . splitOn "\n\n"
  where
    registers = to3Tuple . map (read . last . splitOn ": ") . sort . lines
    program = map read . splitOn "," . last . splitOn ": "

run :: Int -> (Int, Int, Int) -> [Int] -> [Int]
run idx (a, b, c) program
  | idx >= length program = []
  | otherwise = case opcode of
      0 ->
        let a' = a `div` (2 ^ combo operand)
         in run (idx + 2) (a', b, c) program
      1 ->
        let b' = b `xor` operand
         in run (idx + 2) (a, b', c) program
      2 ->
        let b' = combo operand `mod` 8
         in run (idx + 2) (a, b', c) program
      3 ->
        let ptr = (if a == 0 then idx + 2 else operand)
         in run ptr (a, b, c) program
      4 ->
        let b' = b `xor` c
         in run (idx + 2) (a, b', c) program
      5 ->
        let val = combo operand `mod` 8
         in val : run (idx + 2) (a, b, c) program
      6 ->
        let b' = a `div` (2 ^ combo operand)
         in run (idx + 2) (a, b', c) program
      7 ->
        let c' = a `div` (2 ^ combo operand)
         in run (idx + 2) (a, b, c') program
      _ -> error $ "Unexpected opcode " ++ show opcode
  where
    combo x
      | x `elem` [0 .. 3] = x
      | x == 4 = a
      | x == 5 = b
      | x == 6 = c
      | otherwise = error $ "Invalid combo operand " ++ show x

    (opcode : operand : _) = drop idx program

part1 :: String -> String
part1 = intercalate "," . map show . uncurry (run 0) . parse

part2 :: String -> Int
part2 s = head $ solve (length program - 1) 0
  where
    ((_, b, c), program) = parse s

    solve curPower acc
      | curPower < 0 = [acc]
      | otherwise =
          concat
            [ solve (curPower - 1) acc'
              | x <- [0 .. 7],
                let acc' = acc + x * 8 ^ curPower,
                drop curPower program == drop curPower (run 0 (acc', b, c) program)
            ]
