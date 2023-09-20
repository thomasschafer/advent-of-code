import Debug.Trace
import Distribution.Simple.Setup (falseArg)

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
        str' -> word : splitBy splitChar remainder
          where
            (word, remainder) = break splitFn str'

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
    boards = groupBoards $ drop 2 dataLines

findWinner :: [BingoBoard] -> Maybe BingoBoard
findWinner [] = Nothing
findWinner (board : boards) =
  if isWinner board then Just board else findWinner boards
  where
    hasCompleteRow = any (all (== 0))

    hasCompleteColumn [] = False
    hasCompleteColumn board = case head board of
      [] -> False
      _ -> firstColIsComplete || hasCompleteColumn (map tail board)
        where
          firstCol = map head board
          firstColIsComplete = all (== head firstCol) firstCol

    isWinner b = hasCompleteRow b || hasCompleteColumn b

removeNum :: Int -> BingoBoard -> BingoBoard
removeNum num = map $ map (\x -> if x == num then 0 else x)

solvePart1 :: BingoData -> Int
solvePart1 bingoData =
  let (numCalled : remainder) = drawnNumbers bingoData
      updatedBoards = map (removeNum numCalled) (boards bingoData)
   in case findWinner updatedBoards of
        Nothing -> solvePart1 $ BingoData remainder updatedBoards
        Just winningBoard -> numCalled * sum (map sum winningBoard)

solveAndPrint :: String -> IO ()
solveAndPrint filePath = do
  bingoData <- readFile filePath
  print $ solvePart1 $ parseBingoData bingoData

main :: IO ()
main = do
  solveAndPrint "data/day_4_test.txt"
  solveAndPrint "data/day_4.txt"
