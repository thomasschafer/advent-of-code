module Day16 (day16Main) where

import Data.Maybe (mapMaybe)
import Numeric (readHex)
import Text.Printf (printf)
import Utils (binaryStringToInt)

hexToBinary :: String -> String
hexToBinary = concat . mapMaybe hexCharToBin
  where
    hexCharToBin c =
      case readHex [c] of
        (x, _) : _ -> Just $ printf "%04b" (x :: Int)
        _ -> Nothing

-- Packet header:
-- -- First three bits = packet version
-- -- Next three bits encode the packet type ID
-- May contain some extra 0s at end, to be ignored
-- If type ID is 4, then literal value (single number):
-- -- pad with leading 0s until length is a multiple of 4 bits
-- -- break into groups of 4 bits
-- -- prefix each group by 1 bit except last, prefix by 0
-- If type ID is not 4, then operator:
-- -- Packet header (as before)
-- -- Next bit is length type ID:
-- -- | 0 -> next 15 bits are a number representing total length in bits of subpackets
-- -- | 1 -> next 11 bits are number of sub-packets immediately contained
-- -- Subpackets

sumOfVersionNums :: Maybe Int -> Int -> String -> (Int, String)
sumOfVersionNums numPacketsToParse acc binStr
  | maybe False (<= 0) numPacketsToParse = (acc, binStr)
  | length binStr < 7 = (acc, "")
  | otherwise = sumOfVersionNums newNumPacketsToParse (acc + versionNum + containedSumOfVerNums) remainingPackets
  where
    newNumPacketsToParse = case numPacketsToParse of
      Just x -> Just $ x - 1
      _ -> Nothing
    packetId = binaryStringToInt $ take 3 $ drop 3 binStr
    versionNum = binaryStringToInt $ take 3 binStr
    remainingBinStr = drop 6 binStr
    (containedSumOfVerNums, remainingPackets) = case packetId of
      4 -> (0, snd $ extractLiteral ("", remainingBinStr))
        where
          extractLiteral :: (String, String) -> (String, String)
          extractLiteral (curLiteral, remainder)
            | length remainder < 5 = (curLiteral, "")
            | head remainder == '0' = (newLiteral, rest)
            | otherwise = extractLiteral (newLiteral, rest)
            where
              newLiteral = curLiteral ++ take 5 remainder
              rest = drop 5 remainder
      _ -> case lengthTypeId of
        '0' -> (contained, if not (null remainder) then error ("Expected empty remainder, found " ++ remainder) else afterSubPackets)
          where
            lengthOfSubPackets = binaryStringToInt $ take 15 rest
            subPackets = take lengthOfSubPackets $ drop 15 rest
            afterSubPackets = drop (lengthOfSubPackets + 15) rest
            (contained, remainder) = sumOfVersionNums Nothing 0 subPackets
        '1' -> (contained, afterSubPackets)
          where
            numSubPackets = binaryStringToInt $ take 11 rest
            subPacketsAndRest = drop 11 rest
            (contained, afterSubPackets) = sumOfVersionNums (Just numSubPackets) 0 subPacketsAndRest
        _ -> error ("expected length type ID to be 0 or 1, instead found" ++ [lengthTypeId])
        where
          lengthTypeId = head remainingBinStr
          rest = tail remainingBinStr

solvePart1 :: String -> Int
solvePart1 hexString = if null remainingPackets then result else error ("Expected empty remainingPackets, found" ++ remainingPackets)
  where
    binaryString = hexToBinary hexString
    (result, remainingPackets) = sumOfVersionNums Nothing 0 binaryString

day16Main :: IO ()
day16Main = do
  testDataList <- readFile "data/day_16_test.txt"
  realData <- readFile "data/day_16.txt"
  mapM_ (print . solvePart1) (lines testDataList) -- Expected: 16, 12, 23, 31
  print $ solvePart1 realData

-- print $ solvePart2 testData
-- print $ solvePart2 realData
