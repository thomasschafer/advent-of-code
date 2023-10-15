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

extractLiteral :: (String, String) -> (String, String)
extractLiteral (curLiteral, remainder)
  | length remainder < 5 = (curLiteral, "")
  | head remainder == '0' = (newLiteral, rest)
  | otherwise = extractLiteral (newLiteral, rest)
  where
    newLiteral = curLiteral ++ take 5 remainder
    rest = drop 5 remainder

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

-- Potential ideas:
-- 1. Return array of ints, operator performs operation
-- 2. Parse operators and values into a tree in first pass, second pass performs computation

packetIdToOperator :: Int -> ([Int] -> Int)
packetIdToOperator packetId = case packetId of
  0 -> sum
  1 -> product
  2 -> minimum
  3 -> maximum
  5 -> greaterThan
  6 -> lessThan
  7 -> equalTo
  _ -> error ("Could not match packet ID " ++ show packetId ++ " to operator")
  where
    greaterThan [a, b] = if a > b then 1 else 0
    greaterThan xs = error ("Expected length 2, instead found " ++ show xs)

    lessThan [a, b] = if a < b then 1 else 0
    lessThan xs = error ("Expected length 2, instead found " ++ show xs)

    equalTo [a, b] = if a == b then 1 else 0
    equalTo xs = error ("Expected length 2, instead found " ++ show xs)

parseAndSolve :: Maybe Int -> String -> ([Int], String)
parseAndSolve numPacketsToParse binStr
  | maybe False (<= 0) numPacketsToParse = ([], binStr)
  | length binStr < 7 = ([], "")
  | otherwise = (containedResult : packetRest, remainderToParse)
  where
    (packetRest, remainderToParse) = parseAndSolve newNumPacketsToParse remainingPackets
    newNumPacketsToParse = case numPacketsToParse of
      Just x -> Just $ x - 1
      _ -> Nothing
    packetId = binaryStringToInt $ take 3 $ drop 3 binStr
    remainingBinStr = drop 6 binStr
    (containedResult, remainingPackets) = case packetId of
      4 -> (binaryStringToInt literalStr, remaining)
        where
          (literalStr, remaining) = extractLiteral ("", remainingBinStr)
      _ -> case lengthTypeId of
        '0' ->
          ( operator contained,
            if not (null remainder) then error ("Expected empty remainder, found " ++ remainder) else afterSubPackets
          )
          where
            lengthOfSubPackets = binaryStringToInt $ take 15 rest
            subPackets = take lengthOfSubPackets $ drop 15 rest
            afterSubPackets = drop (lengthOfSubPackets + 15) rest
            (contained, remainder) = parseAndSolve Nothing subPackets
        '1' -> (operator contained, afterSubPackets)
          where
            numSubPackets = binaryStringToInt $ take 11 rest
            subPacketsAndRest = drop 11 rest
            (contained, afterSubPackets) = parseAndSolve (Just numSubPackets) subPacketsAndRest
        _ -> error ("expected length type ID to be 0 or 1, instead found" ++ [lengthTypeId])
        where
          lengthTypeId = head remainingBinStr
          rest = tail remainingBinStr
          operator = packetIdToOperator packetId

solvePart2 :: String -> Int
solvePart2 hexString
  | not (null remainingPackets) = error ("Expected empty remainingPackets, found " ++ remainingPackets)
  | length result /= 1 = error ("Expected result to have length 1, instead found " ++ show result)
  | otherwise = head result
  where
    binaryString = hexToBinary hexString
    (result, remainingPackets) = parseAndSolve Nothing binaryString

day16Main :: IO ()
day16Main = do
  testData1List <- readFile "data/day_16_test_1.txt"
  realData <- readFile "data/day_16.txt"
  mapM_ (print . solvePart1) (lines testData1List) -- Expected: 16, 12, 23, 31
  print $ solvePart1 realData
  testData2List <- readFile "data/day_16_test_2.txt"
  mapM_ (print . solvePart2) (lines testData2List) -- Expected: 3, 54, 7, 9, 1, 0, 0, 1
  print $ solvePart2 realData
