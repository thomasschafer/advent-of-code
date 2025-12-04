module Day04 (part1, part2) where

parse :: String -> [[Bool]]
parse = map (map (== '@')) . lines

accessibleRolls :: [[Bool]] -> Int
accessibleRolls rolls =
  length
    [ ()
    | r <- [0 .. length rolls - 1],
      c <- [0 .. length (head rolls) - 1],
      rolls !! r !! c && isAccessible r c
    ]
  where
    isAccessible r c = length neighbouringRolls < 4
      where
        neighbouringRolls =
          [ ()
          | i <- [-1 .. 1],
            j <- [-1 .. 1],
            let r' = r + i,
            let c' = c + j,
            r' >= 0 && r' < length rolls && c' >= 0 && c' < length (head rolls) && (r', c') /= (r, c),
            rolls !! r' !! c'
          ]

part1 :: String -> Int
part1 = accessibleRolls . parse

part2 :: String -> Int
part2 = const 2
