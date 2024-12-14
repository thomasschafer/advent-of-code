module Day14 (part1Test, part1Full, part2Test, part2Full) where

import Data.List.Split (splitOn)
import Data.Set qualified as S
import Utils (mapTuple, toTuple)

parse :: String -> ((Int, Int), (Int, Int))
parse = toTuple . map (toTuple . map read . splitOn "," . last . splitOn "=") . words

move :: (Int, Int) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
move (width, height) ((px, py), (vx, vy)) = (((px + vx) `mod` width, (py + vy) `mod` height), (vx, vy))

safetyFactor :: (Int, Int) -> [(Int, Int)] -> Int
safetyFactor (width, height) robotPositions = product $ map length quadrants
  where
    (quadHeight, quadWidth) = mapTuple (`div` 2) (height, width)
    quadrants =
      [ filter (\(x, y) -> x `f` quadWidth && y `g` quadHeight) robotPositions
        | let comps = [(<), (>)],
          f <- comps,
          g <- comps
      ]

part1 :: (Int, Int) -> String -> Int
part1 dims = safetyFactor dims . map fst . (!! 100) . iterate (map $ move dims) . map parse . lines

part1Test, part1Full :: String -> Int
(part1Test, part1Full) = mapTuple part1 ((11, 7), (101, 103))

display :: (Int, Int) -> Int -> [((Int, Int), (Int, Int))] -> IO ()
display (width, height) n robots = do
  let positionsSet = S.fromList $ map fst robots
  let positions =
        [ [ if (r, c) `elem` positionsSet then '#' else ' '
            | r <- [0 .. width - 1]
          ]
          | c <- [0 .. height - 1]
        ]
  let s = replicate 20 '-'
  print $ s ++ show n ++ s
  mapM_ print positions
  print $ replicate 2 s

part2 :: (Int, Int) -> String -> IO ()
part2 dims = mapM_ (uncurry $ display dims) . take 100000000 . zip [0 ..] . iterate (map $ move dims) . map parse . lines

part2Test, part2Full :: String -> IO ()
(part2Test, part2Full) = mapTuple part2 ((11, 7), (101, 103))
