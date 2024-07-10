import Day04 (part1, part2)

main :: IO ()
main = do
  realData <- readFile "data/day_04.txt"
  print $ part1 realData
  print $ part2 realData
