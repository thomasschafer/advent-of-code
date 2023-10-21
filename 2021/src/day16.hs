module Day16 (day16Main) where

import Data.Maybe (mapMaybe)
import Numeric (readHex)
import Text.Parsec (count, digit, many1, parse, string, try, (<|>))
import Text.ParserCombinators.Parsec (Parser)
import Text.Printf (printf)
import Utils (binaryStringToInt)

data Packet
  = Literal {version :: Int, typeId :: Int, value :: Int}
  | Operator {version :: Int, typeId :: Int, subPackets :: [Packet]}

hexToBinary :: String -> String
hexToBinary = concat . mapMaybe hexCharToBin
  where
    hexCharToBin c =
      case readHex [c] of
        (x, _) : _ -> Just $ printf "%04b" (x :: Int)
        _ -> Nothing

-- Parsing logic copied from https://www.reddit.com/r/adventofcode/comments/rhj2hm/2021_day_16_solutions/hor1pwl

parsePacket :: Parser Packet
parsePacket = try parseLiteral <|> parseOperator

parseLiteral :: Parser Packet
parseLiteral = do
  version <- binaryStringToInt <$> count 3 digit
  typeId <- binaryStringToInt <$> string "100"
  value <- binaryStringToInt <$> parseLiteralNum
  return $ Literal {version, typeId, value}

parseLiteralNum :: Parser String
parseLiteralNum = do
  header <- digit
  case header of
    '1' -> do
      groupOf4 <- count 4 digit
      rest <- parseLiteralNum
      return $ groupOf4 ++ rest
    '0' -> count 4 digit
    other -> error $ "parseLiteralNum: Expected 0 or 1, found " ++ [other]

parseOperator :: Parser Packet
parseOperator = do
  version <- binaryStringToInt <$> count 3 digit
  typeId <- binaryStringToInt <$> count 3 digit
  lengthTypeId <- binaryStringToInt <$> count 1 digit
  case lengthTypeId of
    0 -> do
      totalLenBits <- binaryStringToInt <$> count 15 digit
      toParse <- count totalLenBits digit
      case parse (many1 parsePacket) "Operator" toParse of
        Right subPackets -> return $ Operator {version, typeId, subPackets}
        Left err -> error $ show err
    1 -> do
      numSubPackets <- binaryStringToInt <$> count 11 digit
      subPackets <- count numSubPackets parsePacket
      return $ Operator {version, typeId, subPackets}
    other -> error $ "parseOperator: Expected 0 or 1, instead found " ++ show other

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

versionSum :: Packet -> Int
versionSum Literal {version} = version
versionSum Operator {version, subPackets} = version + sum (versionSum <$> subPackets)

applyOperators :: Packet -> Int
applyOperators Literal {value} = value
applyOperators Operator {typeId, subPackets} = operatorFunc $ map applyOperators subPackets
  where
    operatorFunc = packetIdToOperator typeId

solve :: (Packet -> Int) -> String -> Int
solve func hexString = case parse parsePacket "Parser 2" binaryString of
  Right packet -> func packet
  Left err -> error $ show err
  where
    binaryString = hexToBinary hexString

solvePart1 :: String -> Int
solvePart1 = solve versionSum

solvePart2 :: String -> Int
solvePart2 = solve applyOperators

day16Main :: IO ()
day16Main = do
  testData1List <- readFile "data/day_16_test_1.txt"
  realData <- readFile "data/day_16.txt"
  mapM_ (print . solvePart1) (lines testData1List)
  print $ solvePart1 realData
  testData2List <- readFile "data/day_16_test_2.txt"
  mapM_ (print . solvePart2) (lines testData2List)
  print $ solvePart2 realData
