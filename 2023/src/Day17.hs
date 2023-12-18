module Day17 (part1, part2) where

import Data.Char (digitToInt)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.PQueue.Min (MinQueue)
import Data.PQueue.Min qualified as MQ
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

-- TODO TIDY
findValidNeighbors :: Int -> Int -> [[Int]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findValidNeighbors minRun maxRun cityMap (r, c) prev =
  filter
    (\(r', c') -> r' >= 0 && r' < length cityMap && c' >= 0 && c' < length (head cityMap))
    validNeighbors
  where
    validNeighbors
      | length prev > maxRun =
          error ("Expected length of prev <= " ++ show maxRun ++ ", found " ++ show prev ++ " for " ++ show (r, c))
      | null prev = allNeighbors
      | runLength == maxRun = filter (/= directionContinue) neighbors
      | runLength < minRun = [directionContinue]
      | otherwise = neighbors

    allNeighbors = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
    deltas = zipWith (\(r1, c1) (r2, c2) -> (r1 - r2, c1 - c2)) ((r, c) : prev) prev
    neighbors = filter (/= head prev) allNeighbors
    (delR, delC) = head deltas
    directionContinue = (r + delR, c + delC)
    runLength = length $ filter (== head deltas) deltas

type Queue = MinQueue (Distance Int, (Int, Int), [(Int, Int)])

type CoordsWithPath = ((Int, Int), [(Int, Int)])

dijkstra :: Int -> Int -> (Int, Int) -> Queue -> Set CoordsWithPath -> HashMap CoordsWithPath (Distance Int) -> [[Int]] -> Int
dijkstra minRun maxRun target queue seen costs cityMap
  | MQ.null queue = error "Found empty queue"
  | nextCoords == target =
      case nextDist of
        Dist dist -> trace (intercalate "\n" [[if (r, c) `elem` path then '#' else (head . show) (cityMap !! r !! c) | c <- [0 .. length (head cityMap) - 1]] | r <- [0 .. length cityMap - 1]]) dist
        Infinity -> error "Expected finite distance, found Infinity"
  | (nextCoords, path) `S.member` seen = dijkstra minRun maxRun target queueWithoutNext seen costs cityMap
  | otherwise = dijkstra minRun maxRun target updatedQueue updatedSeen updatedCosts cityMap
  where
    ((nextDist, nextCoords, path), queueWithoutNext) = MQ.deleteFindMin queue
    updatedSeen = S.insert (nextCoords, path) seen
    -- neighbors = findValidNeighbors minRun maxRun cityMap nextCoords (take maxRun path)
    neighbors = findValidNeighbors minRun maxRun cityMap nextCoords path
    (updatedQueue, updatedCosts) = foldl update (queueWithoutNext, costs) neighbors

    update (queue', costs') neighborCoords@(neighRow, neighCol) =
      if altDist < curDist
        then
          ( MQ.insert (altDist, neighborCoords, updatedPath) queue',
            HM.insert (neighborCoords, updatedPath) altDist costs'
          )
        else (queue', costs')
      where
        curDist = fromMaybe Infinity $ HM.lookup (neighborCoords, path) costs'
        altDist = addDist nextDist (Dist $ cityMap !! neighRow !! neighCol)
        -- updatedPath = nextCoords : path
        updatedPath = nextCoords : take (maxRun - 1) path

shortestPathToTarget :: Int -> Int -> [[Int]] -> Int
shortestPathToTarget minRun maxRun cityMap = dijkstra minRun maxRun target queue S.empty HM.empty cityMap
  where
    queue =
      MQ.fromList
        [ (if r == 0 && c == 0 then Dist 0 else Infinity, (r, c), [])
          | r <- [0 .. length cityMap - 1],
            c <- [0 .. length (head cityMap) - 1]
        ]
    target = (length cityMap - 1, length (head cityMap) - 1)

solve :: Int -> Int -> String -> Int
solve minRun maxRun = shortestPathToTarget minRun maxRun . map (map digitToInt) . lines

part1 :: String -> Int
part1 = solve 1 3

part2 :: String -> Int
part2 = solve 4 10
