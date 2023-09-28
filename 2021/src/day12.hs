module Day12 (day12Main) where

import Data.Char (isLower)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (splitBy)

buildPathMap :: String -> HashMap String [String]
buildPathMap pathData = pathMap
  where
    paths = map (splitBy '-') (lines pathData)
    pathMap = foldl addMapping HM.empty paths

    addMapping :: HashMap String [String] -> [String] -> HashMap String [String]
    addMapping mapping [node1, node2] = HM.insert node1 updatedNode1Nodes $ HM.insert node2 updatedNode2Nodes mapping
      where
        updatedNode1Nodes = fromMaybe [] (HM.lookup node1 mapping) ++ [node2]
        updatedNode2Nodes = fromMaybe [] (HM.lookup node2 mapping) ++ [node1]
    addMapping mapping _ = mapping

solvePart1 :: String -> Int
solvePart1 pathData = numPathsToEnd S.empty "start"
  where
    pathMap = buildPathMap pathData

    numPathsToEnd :: Set String -> String -> Int
    numPathsToEnd _ "end" = 1
    numPathsToEnd visited curNode = sum numChildPaths
      where
        updatedVisited = if isLower (head curNode) then S.insert curNode visited else visited
        numChildPaths =
          [ numPathsToEnd updatedVisited child
            | child <- fromJust (HM.lookup curNode pathMap),
              child `notElem` visited
          ]

solvePart2 :: String -> Int
solvePart2 pathData = numPathsToEnd S.empty False "start"
  where
    pathMap = buildPathMap pathData

    numPathsToEnd :: Set String -> Bool -> String -> Int
    numPathsToEnd _ _ "end" = 1
    numPathsToEnd visited hasVisSmallTwice curNode = sum numChildPaths
      where
        updatedVisited = if isLower (head curNode) then S.insert curNode visited else visited
        numChildPaths =
          [ numPathsToEnd updatedVisited (hasVisSmallTwice || child `elem` visited) child
            | child <- fromJust (HM.lookup curNode pathMap),
              child /= "start",
              (child `notElem` visited) || not hasVisSmallTwice
          ]

day12Main :: IO ()
day12Main = do
  testData1 <- readFile "data/day_12_test_1.txt"
  testData2 <- readFile "data/day_12_test_2.txt"
  testData3 <- readFile "data/day_12_test_3.txt"
  realData <- readFile "data/day_12.txt"
  print $ solvePart1 testData1
  print $ solvePart1 testData2
  print $ solvePart1 testData3
  print $ solvePart1 realData
  print $ solvePart2 testData1
  print $ solvePart2 testData2
  print $ solvePart2 testData3
  print $ solvePart2 realData
