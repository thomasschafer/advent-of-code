module Day07 (part1, part2) where

import Data.Set (Set)
import Data.Set qualified as S
import Utils (withIndices2d)

data Diagram = Diagram
  { splitters :: Set (Int, Int),
    height :: Int,
    width :: Int
  }

parse :: [String] -> ((Int, Int), Diagram)
parse s = (startPos, diagram)
  where
    withIndices = withIndices2d s
    startPos = fst . head $ filter ((== 'S') . snd) withIndices
    splitters = S.fromList . map fst $ filter ((== '^') . snd) withIndices
    diagram = Diagram {splitters, height = length s, width = length (head s)}

part1 :: String -> Int
part1 = snd . uncurry (run S.empty) . parse . lines
  where
    run :: Set (Int, Int) -> (Int, Int) -> Diagram -> (Set (Int, Int), Int)
    run visited (pos@(r, c)) diagram
      | (pos `S.member` visited) || r < 0 || r >= height diagram || c < 0 || c >= width diagram =
          (visited, 0)
      | pos `elem` splitters diagram =
          let (visitedLeft, numSplitsLeft) = run (S.insert pos visited) (r, c - 1) diagram
              (visitedRight, numSplitsRight) = run visitedLeft (r, c + 1) diagram
           in (visitedRight, 1 + numSplitsLeft + numSplitsRight)
      | otherwise = run (S.insert pos visited) (r + 1, c) diagram

part2 :: String -> Int
part2 = const 2
