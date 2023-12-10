module Day10 (part1, part2) where

import Data.HashMap.Strict qualified as HM
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (freqCounts)
import Prelude hiding (Left, Right)

type Coords = (Int, Int)

allNeighbors :: Int -> Int -> Set Coords -> Coords -> [Coords]
allNeighbors rows cols dontVisit (r, c) =
  [ (r', c')
    | (r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
      0 <= r' && r' < rows,
      0 <= c' && c' < cols,
      (r', c') `notElem` dontVisit
  ]

validNeighbors :: [[Char]] -> Int -> Int -> Set Coords -> Coords -> [Coords]
validNeighbors pipeData rows cols dontVisit (r, c) =
  filter (`notElem` dontVisit) $
    [(r + 1, c) | cur `elem` downNodes, r + 1 < rows, (pipeData !! (r + 1) !! c) `elem` upNodes]
      ++ [(r - 1, c) | cur `elem` upNodes, r - 1 >= 0, (pipeData !! (r - 1) !! c) `elem` downNodes]
      ++ [(r, c + 1) | cur `elem` rightNodes, c + 1 < cols, (pipeData !! r !! (c + 1)) `elem` leftNodes]
      ++ [(r, c - 1) | cur `elem` leftNodes, c - 1 >= 0, (pipeData !! r !! (c - 1)) `elem` rightNodes]
  where
    cur = pipeData !! r !! c
    leftNodes = ['-', '7', 'J', 'S']
    rightNodes = ['-', 'F', 'L', 'S']
    upNodes = ['|', 'L', 'J', 'S']
    downNodes = ['|', 'F', '7', 'S']

findS :: [[Char]] -> Coords
findS pipeData =
  head
    [ (r, c)
      | r <- [0 .. length pipeData - 1],
        c <- [0 .. length (head pipeData) - 1],
        pipeData !! r !! c == 'S'
    ]

findLoop :: [[Char]] -> (Int, Coords)
findLoop pipeData = findLoop' 0 sCoords (S.fromList sCoords)
  where
    sCoords = [findS pipeData]

    findLoop' acc curNodes visited
      | null curNodes = error "Found empty curNodes"
      | length duplicateNodes > 1 = error ("Found multiple duplicate nodes: " ++ show duplicateNodes)
      | length duplicateNodes == 1 = (acc + 1, head duplicateNodes) -- two paths have arrived at the same node, loop found
      | otherwise = findLoop' (acc + 1) newNodes updatedVisited
      where
        newNodes = concatMap (validNeighbors pipeData (length pipeData) (length $ head pipeData) visited) curNodes
        nodeCounts = freqCounts newNodes
        duplicateNodes = map fst . filter ((> 1) . snd) $ HM.toList nodeCounts
        updatedVisited = foldl (\vis (r, c) -> S.insert (r, c) vis) visited newNodes

part1 :: String -> Int
part1 = fst . findLoop . lines

allLoopNodes :: [[Char]] -> Coords -> Set Coords
allLoopNodes pipeData startNode = allLoopNodes' S.empty [startNode]
  where
    rows = length pipeData
    cols = length $ head pipeData

    allLoopNodes' loopNodes curNodes =
      if 'S' `elem` map (\(r, c) -> pipeData !! r !! c) newNodes
        then updatedLoopNodes
        else allLoopNodes' updatedLoopNodes newNodes
      where
        newNodes = concatMap (validNeighbors pipeData rows cols loopNodes) curNodes
        updatedLoopNodes = foldl (\vis (r, c) -> S.insert (r, c) vis) loopNodes newNodes

data Direction = Left | Right | Up | Down
  deriving (Show)

updatedNodesInsideLoop :: ([[Char]], Int, Int) -> Set Coords -> Set Coords -> Coords -> Direction -> Set Coords
updatedNodesInsideLoop (pipeData, rows, cols) loopNodes nodesInsideLoop (curRow, curCol) curDirection =
  updateNodes nodesInsideLoop neighboursInsideLoop
  where
    upNode = (curRow - 1, curCol)
    downNode = (curRow + 1, curCol)
    rightNode = (curRow, curCol + 1)
    leftNode = (curRow, curCol - 1)

    curSym = pipeData !! curRow !! curCol
    neighboursInsideLoop
      | curSym == '-' = case curDirection of
          Left -> [upNode]
          Right -> [downNode]
          _ -> error (show curSym ++ show curDirection)
      | curSym == '|' = case curDirection of
          Up -> [rightNode]
          Down -> [leftNode]
          _ -> error (show curSym ++ show curDirection)
      | curSym == 'L' = case curDirection of
          Right -> [leftNode, downNode]
          Up -> []
          _ -> error (show curSym ++ show curDirection)
      | curSym == 'J' = case curDirection of
          Left -> []
          Up -> [rightNode, downNode]
          _ -> error (show curSym ++ show curDirection)
      | curSym == '7' = case curDirection of
          Left -> [rightNode, upNode]
          Down -> []
          _ -> error (show curSym ++ show curDirection)
      | curSym == 'F' = case curDirection of
          Right -> []
          Down -> [leftNode, upNode]
          _ -> error (show curSym ++ show curDirection)
      | curSym == 'S' = [] -- TODO HANDLE
      | otherwise = error (show curSym)

    updateNodes = foldl update
      where
        update nodes cur =
          if cur `elem` dontVisit
            then nodes
            else foldl update (S.insert cur nodes) $ allNeighbors rows cols dontVisit cur
          where
            dontVisit = S.union nodes loopNodes

tilesEnclosedByLoop :: [[Char]] -> Set Coords -> Int
tilesEnclosedByLoop pipeData loopNodes =
  length $ tilesEnclosedByLoop' S.empty (topLeftMostNode, Right)
  where
    rows = length pipeData
    cols = length $ head pipeData

    -- topLeftMostNode must be an 'F', so we can traverse the loop clockwise from here, and DFS on nodes to the right,
    -- marking as visited. We can then return length of this visited set.
    topLeftMostNode = minimum $ S.toList loopNodes

    tilesEnclosedByLoop' :: Set Coords -> (Coords, Direction) -> Set Coords
    tilesEnclosedByLoop' nodesInsideLoop (curNode@(curRow, curCol), curDirection) =
      if newNode == topLeftMostNode
        then newNodesInsideLoop
        else tilesEnclosedByLoop' newNodesInsideLoop (newNode, newDirection)
      where
        newNode@(newRow, newCol) =
          (\n -> if n `notElem` loopNodes then error ("Eror - n: " ++ show n) else n) $
            case curDirection of
              Left -> (curRow, curCol - 1)
              Right -> (curRow, curCol + 1)
              Up -> (curRow - 1, curCol)
              Down -> (curRow + 1, curCol)

        newNodesInsideLoop = updatedNodesInsideLoop (pipeData, rows, cols) loopNodes nodesInsideLoop (curRow, curCol) curDirection

        newDirection = case pipeData !! fst newNode !! snd newNode of
          newSym
            | newSym `elem` ['|', '-'] -> curDirection
            | newSym == 'L' -> case curDirection of
                Down -> Right
                Left -> Up
                _ -> error ("Unexpected curDirection for " ++ [newSym] ++ ": " ++ show curDirection)
            | newSym == 'J' -> case curDirection of
                Down -> Left
                Right -> Up
                _ -> error ("Unexpected curDirection for " ++ [newSym] ++ ": " ++ show curDirection)
            | newSym == '7' -> case curDirection of
                Up -> Left
                Right -> Down
                _ -> error ("Unexpected curDirection for " ++ [newSym] ++ ": " ++ show curDirection)
            | newSym == 'F' -> case curDirection of
                Up -> Right
                Left -> Down
                _ -> error ("Unexpected curDirection for " ++ [newSym] ++ ": " ++ show curDirection)
            | newSym == 'S' -> snd . fromJust $ find ((/= curNode) . fst) $ filter ((`elem` loopNodes) . fst) possibleNewNewNodes -- TODO handle better
            where
              possibleNewNewNodes =
                [ ((newRow + 1, newCol), Down),
                  ((newRow - 1, newCol), Up),
                  ((newRow, newCol + 1), Right),
                  ((newRow, newCol - 1), Left)
                ]
          s -> error ("Unexpected symbol " ++ [s])

part2 :: String -> Int
part2 pipeStr = tilesEnclosedByLoop pipeData . allLoopNodes pipeData . snd $ findLoop pipeData
  where
    pipeData = lines pipeStr
