module Day03 (part1, part2) where

import Data.Char (isDigit)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (findIndex)
import Data.Maybe (fromJust, fromMaybe)
import Utils (quickTrace)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

indicesToCheck :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
indicesToCheck row colStart colEnd numRows numCols =
  [ (r, c)
    | r <- [row - 1 .. row + 1],
      r >= 0,
      r < numRows,
      c <- [colStart - 1 .. colEnd + 1],
      c >= 0,
      c < numCols,
      not (r == row && c >= colStart && c <= colEnd)
  ]

part1 :: String -> Int
part1 engineSchematicData = partNumSum 0 0 0
  where
    engineSchematic = lines engineSchematicData
    rows = length engineSchematic
    cols = length $ head engineSchematic

    hasSurroundingSymbol :: Int -> Int -> Int -> Bool
    hasSurroundingSymbol row colStart colEnd =
      colStart <= colEnd
        && any
          (isSymbol . (\(r, c) -> engineSchematic !! r !! c))
          (indicesToCheck row colStart colEnd rows cols)

    partNumSum :: Int -> Int -> Int -> Int
    partNumSum r c acc
      | r >= rows = acc
      | isDigit (engineSchematic !! r !! c) =
          let numEndCol = fromMaybe cols (findIndex (not . isDigit) (drop c (engineSchematic !! r) ++ ['.'])) + c - 1
              nextCol = (numEndCol + 1) `mod` cols
              nextRow = if nextCol == 0 then r + 1 else r
              num = read . take (numEndCol - c + 1) $ drop c (engineSchematic !! r)
              includeNum = hasSurroundingSymbol r c numEndCol
           in partNumSum nextRow nextCol (if includeNum then acc + num else acc)
      | otherwise = partNumSum (if c + 1 == cols then r + 1 else r) ((c + 1) `mod` cols) acc

type SymbolsMapping = HashMap (Int, Int) [Int]

part2 :: String -> Int
part2 engineSchematicData = sum . map product . filter ((== 2) . length) $ map snd (HM.toList finalMapping)
  where
    engineSchematic = lines engineSchematicData
    rows = length engineSchematic
    cols = length $ head engineSchematic
    initialMapping =
      HM.fromList
        [ ((r, c), [])
          | r <- [0 .. rows - 1],
            c <- [0 .. cols - 1],
            engineSchematic !! r !! c == '*'
        ]
    finalMapping = findNumsBySymbols 0 0 initialMapping

    findNumsBySymbols :: Int -> Int -> SymbolsMapping -> SymbolsMapping
    findNumsBySymbols r c symbolsMap
      | r >= rows = symbolsMap
      | isDigit (engineSchematic !! r !! c) =
          let numEndCol = fromMaybe cols (findIndex (not . isDigit) (drop c (engineSchematic !! r) ++ ['.'])) + c - 1
              nextCol = (numEndCol + 1) `mod` cols
              nextRow = if nextCol == 0 then r + 1 else r
              num = read . take (numEndCol - c + 1) $ drop c (engineSchematic !! r)

              addIfStar :: SymbolsMapping -> (Int, Int) -> SymbolsMapping
              addIfStar curMapping (r', c') =
                if engineSchematic !! r' !! c' == '*'
                  then HM.insert (r', c') (num : fromJust (HM.lookup (r', c') curMapping)) curMapping
                  else curMapping

              updatedMapping = foldl addIfStar symbolsMap (indicesToCheck r c numEndCol rows cols)
           in findNumsBySymbols nextRow nextCol updatedMapping
      | otherwise = findNumsBySymbols (if c + 1 == cols then r + 1 else r) ((c + 1) `mod` cols) symbolsMap
