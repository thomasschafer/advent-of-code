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

type Queue = MinQueue (Distance Int, (Int, Int), [(Int, Int)])

findValidNeighbors :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findValidNeighbors cityMap (r, c) prev = validNeighbors
  where
    allNeighbors =
      filter
        (\(r', c') -> r' >= 0 && r' < length cityMap && c' >= 0 && c' < length (head cityMap))
        [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

    validNeighbors
      | length prev > 3 = error ("Expected length of prev <=3, found " ++ show prev ++ " for " ++ show (r, c))
      | null prev = allNeighbors
      | length prev == 3 && all (== head deltas) deltas =
          let (delR, delC) = head deltas
           in filter (/= (r + delR, c + delC)) neighbors
      | otherwise = neighbors
      where
        deltas = zipWith (\(r1, c1) (r2, c2) -> (r1 - r2, c1 - c2)) ((r, c) : prev) prev
        neighbors = filter (/= head prev) allNeighbors

dijkstra :: (Int, Int) -> [[Int]] -> Queue -> Set ((Int, Int), [(Int, Int)]) -> HashMap ((Int, Int), [(Int, Int)]) (Distance Int) -> Int
dijkstra target cityMap queue seen costs
  | MQ.null queue = error "Found empty queue"
  | nextCoords == target =
      case nextDist of
        Dist dist -> dist
        Infinity -> error "Expected finite distance, found Infinity"
  | (nextCoords, path) `S.member` seen = dijkstra target cityMap queueWithoutNext seen costs
  | otherwise = dijkstra target cityMap updatedQueue updatedSeen updatedCosts
  where
    ((nextDist, nextCoords, path), queueWithoutNext) = MQ.deleteFindMin queue
    updatedSeen = S.insert (nextCoords, path) seen
    neighbors = findValidNeighbors cityMap nextCoords path
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
        updatedPath = nextCoords : take 2 path

shortestPathToTarget :: [[Int]] -> Int
shortestPathToTarget cityMap = dijkstra target cityMap queue seen costs
  where
    queue =
      MQ.fromList
        [ (if r == 0 && c == 0 then Dist 0 else Infinity, (r, c), [])
          | r <- [0 .. length cityMap - 1],
            c <- [0 .. length (head cityMap) - 1]
        ]
    target = (length cityMap - 1, length (head cityMap) - 1)
    seen = S.empty
    costs = HM.empty

part1 :: String -> Int
part1 = shortestPathToTarget . map (map digitToInt) . lines

part2 :: String -> Int
part2 = const 1
