data Position = Position {depth :: Int, horizontal :: Int}

parseCommand :: [String] -> (String, Int)
parseCommand (direction : magnitude : _) = (direction, read magnitude)

updatePositionPt1 :: Position -> [String] -> Position
updatePositionPt1 prev@(Position depth horizontal) command =
  let (direction, magnitude) = parseCommand command
   in case direction of
        "forward" -> Position depth (horizontal + magnitude)
        "down" -> Position (depth + magnitude) horizontal
        "up" -> Position (depth - magnitude) horizontal
        _ -> prev

solvePart1 :: String -> Int
solvePart1 commandData =
  depth * horizontal
  where
    commands = map words $ lines commandData
    Position depth horizontal = foldl updatePositionPt1 (Position 0 0) commands

data PositionWithAim = PositionWithAim {position :: Position, aim :: Int}

updatePositionPt2 :: PositionWithAim -> [String] -> PositionWithAim
updatePositionPt2 prev@(PositionWithAim (Position depth horizontal) aim) command =
  let (direction, magnitude) = parseCommand command
   in case direction of
        "forward" -> PositionWithAim (Position (depth + aim * magnitude) (horizontal + magnitude)) aim
        "down" -> PositionWithAim (Position depth horizontal) (aim + magnitude)
        "up" -> PositionWithAim (Position depth horizontal) (aim - magnitude)
        _ -> prev

solvePart2 :: String -> Int
solvePart2 commandData =
  depth * horizontal
  where
    commands = map words $ lines commandData
    PositionWithAim (Position depth horizontal) _ = foldl updatePositionPt2 (PositionWithAim (Position 0 0) 0) commands

main :: IO ()
main = do
  day2Data <- readFile "data/day_2.txt"
  print $ solvePart1 day2Data
  print $ solvePart2 day2Data