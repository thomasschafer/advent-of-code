module Day11 (part1, part2) where

data Seat = Floor | Empty | Occupied
  deriving (Eq, Show)

toSeat :: Char -> Seat
toSeat '.' = Floor
toSeat 'L' = Empty
toSeat '#' = Occupied

directions :: [(Int, Int)]
directions = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], not (i == 0 && j == 0)]

updateSeat :: Int -> Bool -> [[Seat]] -> (Int, Int) -> Seat
updateSeat occupiedThreshold extended seats (row, col)
  | cur == Empty && Occupied `notElem` adjacentSeats = Occupied
  | cur == Occupied
      && length (filter (== Occupied) adjacentSeats) >= occupiedThreshold =
      Empty
  | otherwise = cur
  where
    cur = seats !! row !! col
    adjacentSeats =
      concatMap
        (take 1 . (if extended then filter (/= Floor) else id) . followDirection)
        directions
    followDirection (i, j) =
      [ seats !! row' !! col'
        | k <- [1 .. max (length seats) (length $ head seats)],
          let row' = row + k * i,
          let col' = col + k * j,
          row' >= 0,
          row' < length seats,
          col' >= 0,
          col' < length (head seats)
      ]

simulateUntilStable :: ([[Seat]] -> (Int, Int) -> Seat) -> [[Seat]] -> [[Seat]]
simulateUntilStable update = simulateUntilStable'
  where
    simulateUntilStable' seats =
      if updated == seats
        then seats
        else simulateUntilStable' updated
      where
        updated =
          [ [ update seats (i, j)
              | j <- [0 .. length (head seats) - 1]
            ]
            | i <- [0 .. length seats - 1]
          ]

solve :: ([[Seat]] -> (Int, Int) -> Seat) -> String -> Int
solve update = seatsOccupied . simulateUntilStable update . map (map toSeat) . lines
  where
    seatsOccupied = length . filter (== Occupied) . concat

part1 :: String -> Int
part1 = solve (updateSeat 4 False)

part2 :: String -> Int
part2 = solve (updateSeat 5 True)
