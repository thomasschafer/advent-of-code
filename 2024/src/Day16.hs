{-# LANGUAGE DeriveGeneric #-}

module Day16 (part1, part2) where

import Control.Arrow ((&&&))
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
  { distanceMap :: HashMap Node Distance,
    nodeQueue :: MinPrioHeap Distance (Node, [(Int, Int)]),
    optimalPaths :: HashSet (Int, Int),
    bestCost :: Distance
  }
  deriving (Show)

dijkstra :: [[Tile]] -> Node -> (Int, Int) -> (Distance, HashSet (Int, Int))
dijkstra tiles src dest = bestCost &&& optimalPaths $ processQueue initialState
  where
    initialState = DijkstraState initialDistances initialQueue initialPaths Infinity
      where
        initialDistances = HM.singleton src (Dist 0)
        initialQueue = H.fromList [(Dist 0, (src, [fst src]))]
        initialPaths = HS.empty

    processQueue ds@(DijkstraState d0 q0 paths bestCost) = case H.view q0 of
      Just ((score, (node, path)), q1) ->
        if score > (d0 !?? node)
          then processQueue (ds {nodeQueue = q1})
          else
            if fst node == dest && score <= bestCost
              then
                processQueue
                  ( ds
                      { nodeQueue = q1,
                        optimalPaths = HS.union paths (HS.fromList path),
                        bestCost = score
                      }
                  )
              else
                let unvisitedNeighbors =
                      filter (\(((r, c), _), _) -> tiles !! r !! c /= Wall) $
                        weightedNeighbors node
                    newState = foldl (foldNeighbour path score) (ds {nodeQueue = q1}) unvisitedNeighbors
                 in processQueue newState
      _ -> ds

    foldNeighbour currentPath currentScore ds@(DijkstraState d0 q1 paths bestCost) (neighborNode, cost) =
      let altDistance = currentScore + Dist cost
          newPath = fst neighborNode : currentPath
       in if altDistance <= bestCost && altDistance <= (d0 !?? neighborNode)
            then
              DijkstraState
                (HM.insert neighborNode altDistance d0)
                (H.insert (altDistance, (neighborNode, newPath)) q1)
                paths
                bestCost
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

solve :: String -> (Distance, HashSet (Int, Int))
solve input = dijkstra grid (start, East) end
  where
    grid = map (map parse) $ lines input
    (start, end) = mapTuple (head . positionsOf grid . (==)) (Start, End)

part1 :: String -> Int
part1 = (\(Dist x) -> x) . fst . solve

part2 :: String -> Int
part2 = length . snd . solve
