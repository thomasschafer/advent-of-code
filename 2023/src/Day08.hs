module Day08 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)
import Utils (quickTrace)

data Direction = L | R
  deriving (Eq, Show)

data Node = Node {val :: String, left :: String, right :: String}
  deriving (Show)

parseNodes :: String -> ([Direction], HashMap String Node)
parseNodes nodeData = (directions, nodes)
  where
    dirString : _ : nodesLines = lines nodeData
    directions = map (\c -> if c == 'R' then R else if c == 'L' then L else error [c]) dirString
    parseNode s = (head s, Node {val = head s, left = tail (init (s !! 2)), right = init (s !! 3)})
    update ns (key, node) = HM.insert key node ns
    nodes = foldl update HM.empty (map (parseNode . words) nodesLines)

stepsToReachEnd :: (String -> Bool) -> [Direction] -> Node -> HashMap String Node -> Int -> Int
stepsToReachEnd isEnd (nextDir : rest) Node {val, left, right} nodeMapping acc =
  if isEnd val
    then acc
    else stepsToReachEnd isEnd rest nextNode nodeMapping (acc + 1)
  where
    nextNode = fromJust $ HM.lookup (if nextDir == L then left else right) nodeMapping
stepsToReachEnd _ [] _ _ _ = error "Found empty directions"

part1 :: String -> Int
part1 s = stepsToReachEnd (== "ZZZ") (cycle dirs) startNode nodeMapping 0
  where
    (dirs, nodeMapping) = parseNodes s
    startNode = fromJust $ HM.lookup "AAA" nodeMapping

stepsForAllToReachEnd :: [Direction] -> [Node] -> HashMap String Node -> Int -> Int
stepsForAllToReachEnd dirs nodes nodeMapping acc = foldl1 lcm steps
  where
    steps = quickTrace "s" $ map (\n -> stepsToReachEnd ((== 'Z') . last) dirs n nodeMapping acc) nodes

part2 :: String -> Int
part2 s = stepsForAllToReachEnd (cycle dirs) startNodes nodeMapping 0
  where
    (dirs, nodeMapping) = parseNodes s
    nodesEndingInA = filter ((== 'A') . last) (HM.keys nodeMapping)
    startNodes = map (\key -> fromJust $ HM.lookup key nodeMapping) nodesEndingInA
