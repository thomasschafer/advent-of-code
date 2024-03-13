module Day14 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Utils (lpad)

data Bit = Zero | One
  deriving (Eq, Show)

data Instruction = Mask [Maybe Bit] | Write {address :: Int, value :: Int}
  deriving (Show)

toBit :: Int -> Bit
toBit 0 = Zero
toBit 1 = One

toBits :: Int -> [Bit]
toBits = lpad Zero 36 . toBits' []
  where
    toBits' acc x
      | x <= 0 = acc
      | otherwise =
          toBits'
            (toBit (x `mod` 2) : acc)
            (x `div` 2)

fromBit :: Bit -> Int
fromBit Zero = 0
fromBit One = 1

fromBits :: [Bit] -> Int
fromBits = foldl (\acc bit -> 2 * acc + fromBit bit) 0

applyMask :: [Maybe Bit] -> Int -> Int
applyMask mask = fromBits . zipWith applyMask' mask . toBits
  where
    applyMask' Nothing bit = bit
    applyMask' (Just x) _ = x

runProgram :: [Instruction] -> HashMap Int Int
runProgram = fst . foldl applyInstruction (HM.empty, replicate 36 Nothing)
  where
    applyInstruction (memory, _) (Mask newMask) = (memory, newMask)
    applyInstruction (memory, mask) (Write {address, value}) =
      ( HM.insert address (applyMask mask value) memory,
        mask
      )

parseInstr :: String -> Instruction
parseInstr s = case splitOn " = " s of
  ["mask", vals] -> Mask $ map parseBit vals
  [addrStr, valueStr] ->
    Write {address = read (drop 4 $ init addrStr), value = read valueStr}
  where
    parseBit '0' = Just Zero
    parseBit '1' = Just One
    parseBit 'X' = Nothing

part1 :: String -> Int
part1 = sum . runProgram . map parseInstr . lines

part2 :: String -> Int
part2 = const 2