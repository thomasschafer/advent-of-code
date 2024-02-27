module Day08 (part1, part2) where

import Data.Set qualified as S

data Instruction = Acc Int | Jump Int | Noop
  deriving (Show)

accValueAtLoop :: [Instruction] -> Int
accValueAtLoop instructions = accValueAtLoop' 0 0 S.empty
  where
    accValueAtLoop' acc idx seen
      | idx `elem` seen = acc
      | otherwise = flip (uncurry accValueAtLoop') (S.insert idx seen) $ case instructions !! idx of
          Noop -> (acc, idx + 1)
          Acc delta -> (acc + delta, idx + 1)
          Jump jumpBy -> (acc, idx + jumpBy)

parse :: String -> Instruction
parse s = case words s of
  ["acc", num] -> Acc $ toNum num
  ["jmp", num] -> Jump $ toNum num
  ["nop", _] -> Noop
  _ -> error $ "Found unexpected operation " ++ s
  where
    toNum = read . filter (/= '+')

part1 :: String -> Int
part1 = accValueAtLoop . map parse . lines

part2 :: String -> Int
part2 = const 2
