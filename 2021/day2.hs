data Position = Position {depth :: Int, horizontal :: Int}

updatePosition :: Position -> [String] -> Position
updatePosition prev@(Position depth horizontal) subLine =
  let direction = subLine !! 0
      magnitude = read (subLine !! 1) :: Int
   in case direction of
        "forward" -> Position depth (horizontal + magnitude)
        "down" -> Position (depth + magnitude) horizontal
        "up" -> Position (depth - magnitude) horizontal
        _ -> prev

depthHorPosProduct :: String -> Int
depthHorPosProduct subData =
  depth * horizontal
  where
    subLines = map words $ lines subData
    Position depth horizontal = foldl updatePosition (Position 0 0) subLines

main :: IO ()
main = do
  part1Data <- readFile "data/day_2_part_1.txt"
  let part1Result = depthHorPosProduct part1Data
  print part1Result
