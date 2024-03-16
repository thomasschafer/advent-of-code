module Day14 (part1, part2) where

import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Utils (lpad)

data Bit = Zero | One
  deriving (Eq)

data Instruction = Mask [Maybe Bit] | Write {address :: Int, value :: Int}

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

parseInstr :: String -> Instruction
parseInstr s = case splitOn " = " s of
  ["mask", vals] -> Mask $ map parseBit vals
  [addrStr, valueStr] ->
    Write {address = read (drop 4 $ init addrStr), value = read valueStr}
  where
    parseBit '0' = Just Zero
    parseBit '1' = Just One
    parseBit 'X' = Nothing

solve :: Maybe ([Maybe Bit] -> Int -> [Int]) -> Maybe ([Maybe Bit] -> Int -> Int) -> String -> Int -- todo refactor applyMask type
solve addressApplyMask valueApplyMask = sum . runProgram . map parseInstr . lines
  where
    addressApplyMask' = fromMaybe (const (: [])) addressApplyMask
    valueApplyMask' = fromMaybe (const id) valueApplyMask
    runProgram = fst . foldl applyInstruction (HM.empty, replicate 36 Nothing)
      where
        applyInstruction (memory, _) (Mask newMask) = (memory, newMask)
        applyInstruction (memory, mask) (Write {address, value}) =
          ( foldr
              (`HM.insert` valueApplyMask' mask value)
              memory
              (addressApplyMask' mask address),
            mask
          )

part1 :: String -> Int
part1 = solve Nothing (Just applyMask)

applyMaskWithFloat :: [Maybe Bit] -> Int -> [Int]
applyMaskWithFloat mask = map (fromBits . reverse) . applyMaskWithFloat' [[]] . zip mask . toBits
  where
    applyMaskWithFloat' acc [] = acc
    applyMaskWithFloat' acc (bitPair : rest) =
      applyMaskWithFloat' [x : bits | bits <- acc, x <- possibleValues bitPair] rest
      where
        possibleValues b = case b of
          (Just Zero, bit) -> [bit]
          (Just One, _) -> [One]
          (Nothing, _) -> [Zero, One]

part2 :: String -> Int
part2 = solve (Just applyMaskWithFloat) Nothing
