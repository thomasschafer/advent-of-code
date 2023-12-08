module Day08 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)

data Direction = L | R
  deriving (Eq)

data Node = Node {val :: String, left :: String, right :: String}

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

solve :: (String -> Bool) -> (String -> Bool) -> String -> Int
solve startPred endPred s = foldl1 lcm stepsForNodes
  where
    (dirs, nodeMapping) = parseNodes s
    startNodes = HM.elems $ HM.filterWithKey (\k _ -> startPred k) nodeMapping
    stepsForNodes = map (\n -> stepsToReachEnd endPred (cycle dirs) n nodeMapping 0) startNodes

part1 :: String -> Int
part1 = solve (== "AAA") (== "ZZZ")

part2 :: String -> Int
part2 = solve ((== 'A') . last) ((== 'Z') . last)