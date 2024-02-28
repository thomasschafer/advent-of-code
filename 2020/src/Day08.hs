module Day08 (part1, part2) where

import Data.Foldable (find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as S

data Instruction = Acc Int | Jump Int | Noop Int

data EndState = Looped | Finished
  deriving (Eq)

accValueAtLoop :: [Instruction] -> (EndState, Int)
accValueAtLoop instructions = accValueAtLoop' 0 0 S.empty
  where
    len = length instructions
    accValueAtLoop' acc idx seen
      | idx >= len = (Finished, acc)
      | idx `elem` seen = (Looped, acc)
      | otherwise = flip (uncurry accValueAtLoop') (S.insert idx seen) $ case instructions !! idx of
          Noop _ -> (acc, idx + 1)
          Acc delta -> (acc + delta, idx + 1)
          Jump jumpBy -> (acc, idx + jumpBy)

parseInstruction :: String -> Instruction
parseInstruction = parse . words
  where
    parse [s, num] = toInstr s (read $ filter (/= '+') num)

    toInstr "acc" = Acc
    toInstr "jmp" = Jump
    toInstr "nop" = Noop

part1 :: String -> Int
part1 = fromLooped . accValueAtLoop . map parseInstruction . lines
  where
    fromLooped (Looped, acc) = acc

modifyAtIdx :: [Instruction] -> Int -> Maybe [Instruction]
modifyAtIdx instructions idx = case instructions !! idx of
  Acc _ -> Nothing
  Jump x -> insert (Noop x)
  Noop x -> insert (Jump x)
  where
    insert x = Just $ take idx instructions ++ [x] ++ drop (idx + 1) instructions

terminationValue :: [Instruction] -> Maybe Int
terminationValue instructions =
  fmap snd
    . find ((== Finished) . fst)
    . mapMaybe (fmap accValueAtLoop . modifyAtIdx instructions)
    $ [0 .. (length instructions - 1)]

part2 :: String -> Int
part2 = fromJust . terminationValue . map parseInstruction . lines
