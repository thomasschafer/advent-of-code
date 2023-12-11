module Day11 (part1, part2) where

parseUniverse :: [[Char]] -> ([(Int, Int)], [Int], [Int])
parseUniverse galaxyData = (galaxies, emptyRows, emptyCols)
  where
    rows = length galaxyData
    cols = length (head galaxyData)
    galaxies =
      filter
        (\(r, c) -> (galaxyData !! r !! c) == '#')
        [(r, c) | c <- [0 .. cols - 1], r <- [0 .. rows - 1]]
    emptyRows =
      filter
        (\r -> all (== '.') (galaxyData !! r))
        [0 .. rows - 1]
    emptyCols =
      filter
        (\c -> all (== '.') [galaxyData !! r !! c | r <- [0 .. rows - 1]])
        [0 .. cols - 1]

distancesBetweenGalaxies :: Int -> ([(Int, Int)], [Int], [Int]) -> [Int]
distancesBetweenGalaxies expansionFactor (galaxies, emptyRows, emptyCols) =
  [ galaxyDist (galaxies !! g1) (galaxies !! g2)
    | g1 <- [1 .. length galaxies - 1],
      g2 <- [0 .. g1 - 1]
  ]
  where
    galaxyDist (r1, c1) (r2, c2) =
      (cMax - cMin + rMax - rMin)
        + (expansionFactor - 1)
          * ( length (filter (\r -> rMin < r && r < rMax) emptyRows)
                + length (filter (\c -> cMin < c && c < cMax) emptyCols)
            )
      where
        cMin = min c1 c2
        cMax = max c1 c2
        rMin = min r1 r2
        rMax = max r1 r2

part1 :: String -> Int
part1 = sum . distancesBetweenGalaxies 2 . parseUniverse . lines

part2 :: String -> Int
part2 = sum . distancesBetweenGalaxies 1000000 . parseUniverse . lines
