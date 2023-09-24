module Day4 (day4Main) where

import Utils (splitBy)

type BingoBoard = [[Int]]

data BingoData = BingoData
  { drawnNumbers :: [Int],
    boards :: [BingoBoard]
  }
  deriving (Show)

groupBoards :: [String] -> [BingoBoard]
groupBoards = groupBoardsHelper [] []
  where
    groupBoardsHelper curBoard boards [] = boards ++ [curBoard]
    groupBoardsHelper curBoard boards (line : remainingLines) = case line of
      "" -> groupBoardsHelper [] (boards ++ [curBoard]) remainingLines
      _ -> groupBoardsHelper (curBoard ++ [map read (words line)]) boards remainingLines

parseBingoData :: String -> BingoData
parseBingoData fileData = BingoData {drawnNumbers, boards}
  where
    dataLines = lines fileData
    drawnNumbers = map read $ splitBy ',' $ head dataLines
    boards = groupBoards $ drop 2 dataLines

hasCompleteRow :: BingoBoard -> Bool
hasCompleteRow = any (all (== 0))

hasCompleteColumn :: BingoBoard -> Bool
hasCompleteColumn [] = False
hasCompleteColumn board = case head board of
  [] -> False
  _ -> firstColIsComplete || hasCompleteColumn (map tail board)
    where
      firstCol = map head board
      firstColIsComplete = all (== head firstCol) firstCol

isWinner :: BingoBoard -> Bool
isWinner b = hasCompleteRow b || hasCompleteColumn b

findWinner :: [BingoBoard] -> Maybe BingoBoard
findWinner [] = Nothing
findWinner (board : boards) = if isWinner board then Just board else findWinner boards

removeNum :: Int -> BingoBoard -> BingoBoard
removeNum num = map $ map (\x -> if x == num then 0 else x)

calcResult :: Int -> BingoBoard -> Int
calcResult numCalled winningBoard = numCalled * sum (map sum winningBoard)

solvePart1 :: BingoData -> Int
solvePart1 bingoData =
  let (numCalled : remainder) = drawnNumbers bingoData
      updatedBoards = map (removeNum numCalled) (boards bingoData)
   in case findWinner updatedBoards of
        Nothing -> solvePart1 $ BingoData remainder updatedBoards
        Just winningBoard -> calcResult numCalled winningBoard

removeWinners :: [BingoBoard] -> [BingoBoard]
removeWinners = filter (not . isWinner)

solvePart2 :: BingoData -> Int
solvePart2 bingoData = case length filteredBoards of
  0 -> calcResult numCalled $ head updatedBoards
  _ -> solvePart2 $ BingoData remainder filteredBoards
  where
    (numCalled : remainder) = drawnNumbers bingoData
    updatedBoards = map (removeNum numCalled) (boards bingoData)
    filteredBoards = removeWinners updatedBoards

parseData :: String -> IO BingoData
parseData filePath = do
  bingoData <- readFile filePath
  return $ parseBingoData bingoData

day4Main :: IO ()
day4Main = do
  testData <- parseData "data/day_4_test.txt"
  realData <- parseData "data/day_4.txt"
  print $ solvePart1 testData
  print $ solvePart1 realData
  print $ solvePart2 testData
  print $ solvePart2 realData
