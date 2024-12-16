{-# LANGUAGE DeriveGeneric #-}

module Day16 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Utils (mapTuple, positionsOf, (...))

data Distance = Dist Int | Infinity
  deriving (Show, Eq)

instance Ord Distance where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

instance Num Distance where
  Dist x + Dist y = Dist $ x + y
  _ + _ = Infinity

type Node = ((Int, Int), Direction)

(!??) :: (Hashable k) => HashMap k Distance -> k -> Distance
(!??) = fromMaybe Infinity ... flip HM.lookup

data DijkstraState = DijkstraState
  { visitedSet :: HashSet Node,
    distanceMap :: HashMap Node Distance,
    nodeQueue :: MinPrioHeap Distance Node
  }
  deriving (Show)

dijkstra :: [[Tile]] -> Node -> (Int, Int) -> Distance
dijkstra tiles src dest = processQueue initialState
  where
    initialState = DijkstraState initialVisited initialDistances initialQueue
      where
        initialVisited = HS.empty
        initialDistances = HM.singleton src (Dist 0)
        initialQueue = H.fromList [(Dist 0, src)]

    processQueue :: DijkstraState -> Distance
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Just ((cost, node), q1) ->
        if fst node == dest
          then cost
          else
            if HS.member node v0
              then processQueue (ds {nodeQueue = q1})
              else
                let v1 = HS.insert node v0
                    unvisitedNeighbors =
                      filter (\(n@((r, c), _), _) -> not (HS.member n v1 || tiles !! r !! c == Wall)) $
                        weightedNeighbors node
                 in processQueue $ foldl (foldNeighbour node) (DijkstraState v1 d0 q1) unvisitedNeighbors
      _ -> Infinity

    foldNeighbour current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = (d0 !?? current) + Dist cost
       in if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (HM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
            else ds

weightedNeighbors :: Node -> [(Node, Int)]
weightedNeighbors (pos@(r, c), direction) =
  ((nextStep, direction), 1)
    : map (\d -> ((pos, d), 1000)) (filter (/= direction) [North, South, East, West])
  where
    nextStep = case direction of
      North -> (r - 1, c)
      South -> (r + 1, c)
      East -> (r, c + 1)
      West -> (r, c - 1)

data Direction = North | South | East | West
  deriving (Eq, Generic, Show)

instance Hashable Direction

data Tile = Empty | Wall | Start | End
  deriving (Eq)

parse :: Char -> Tile
parse '#' = Wall
parse '.' = Empty
parse 'S' = Start
parse 'E' = End

solve :: [[Tile]] -> Int
solve grid = fromDist $ dijkstra grid (start, East) end
  where
    (start, end) = mapTuple (head . positionsOf grid . (==)) (Start, End)
    fromDist (Dist x) = x

part1 :: String -> Int
part1 = solve . map (map parse) . lines

part2 :: String -> Int
part2 = const 2
