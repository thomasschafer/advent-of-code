import Debug.Trace

type BingoBoard = [[Int]]

data BingoData = BingoData
  { drawnNumbers :: [Int],
    boards :: [BingoBoard]
  }
  deriving (Show)

quickTrace :: Show a => a -> a
quickTrace x = trace ("debug: " ++ show x) x

splitBy :: Char -> String -> [String]
splitBy splitChar str =
  let splitFn = (== splitChar)
   in case dropWhile splitFn str of
        "" -> []
        _ -> word : splitBy splitChar remainder
          where
            (word, remainder) = break splitFn str

groupBoards :: [String] -> [BingoBoard]
groupBoards = groupBoardsHelper [] []
  where
    groupBoardsHelper curBoard boards [] = boards ++ [curBoard]
    groupBoardsHelper curBoard boards (line : lines) = case line of
      "" -> groupBoardsHelper [] (boards ++ [curBoard]) lines
      _ -> groupBoardsHelper (curBoard ++ [map read (words line)]) boards lines

parseBingoData :: String -> BingoData
parseBingoData fileData = BingoData {drawnNumbers, boards}
  where
    dataLines = lines fileData
    drawnNumbers = map read $ splitBy ',' $ head dataLines
    boards = quickTrace $ groupBoards $ drop 2 dataLines

-- TODO: implement
solvePart1 :: BingoData -> Int
solvePart1 bingoData = head $ drawnNumbers d
  where
    d = quickTrace bingoData

solveAndPrint :: String -> IO ()
solveAndPrint filePath = do
  bingoData <- readFile filePath
  print $ solvePart1 $ parseBingoData bingoData

main :: IO ()
main = do
  solveAndPrint "data/day_4_test.txt"
